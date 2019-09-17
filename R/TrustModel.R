# TrustModel.r
# An implentation of the trust model proposed in
#
# Author: Cody Lewis
# Date: 2019-09-06

# Compute the probability of a occuring among a, b, and c
compute.probability <- function(a, b, c)
{
    return (sum(a, 1) / sum(a, b, c, 3))
}


# Compute the entropy from the given probabilities
compute.entropy <- function(probabilities)
{
    return (-sum(ent.unit(probabilities)))
}


# Calculate the small unit of entropy
ent.unit <- function(probability)
{
    return (probability * log(probability, base=3))
}


# Do the same as ent.unit but divide the contexts of the log by divider
ent.unit.div <- function(probability, divider)
{
    return (probability * log(probability / divider, base=3))
}


# Find the trust of a device given amounts of trusted, distrusted, and unknown
# services they have provided
compute.trust <- function(trust, distrust, unknown)
{
    prob.trust <- compute.probability(trust, distrust, unknown)
    prob.distrust <- compute.probability(distrust, trust, unknown)
    prob.unknown <- compute.probability(unknown, trust, distrust)

    if ((prob.trust > prob.unknown) && (prob.unknown >= prob.distrust)) {
        return (1 - compute.entropy(c(prob.trust, prob.distrust, prob.unknown)))
    } else if ((prob.trust > prob.distrust) && (prob.distrust > prob.unknown)) {
        return (1 - abs(ent.unit(prob.trust) + ent.unit.div(prob.unknown + prob.distrust, 2)))
    } else if (prob.unknown >= max(prob.trust, prob.distrust)) {
        return (abs(ent.unit(prob.trust) + ent.unit.div(prob.unknown + prob.distrust, 2)) - 1)
    } else if ((prob.distrust >= prob.unknown) && (prob.unknown >= prob.trust)) {
        return (compute.entropy(c(prob.trust, prob.distrust, prob.unknown)) - 1)
    } else {
        return (abs(ent.unit(prob.distrust) + ent.unit.div(prob.unknown + prob.trust, 2)) - 1)
    }
}


# Find the weighted average of the context values (given a vector of one of the 4)
weighted.avg.context <- function(theta, contexts)
{
    context.latest <- tail(contexts, 1)
    factor.forget <- theta**abs(context.latest - head(contexts, -1))
    return ((context.latest + sum(factor.forget * head(contexts, -1))) / (1 + sum(factor.forget)))
}


# Find the distance between the target context, and the weighted context
context.distance <- function(weights, context.target, context.weighted)
{
    return (sqrt(sum(weights * (context.target - context.weighted)**2)))
}


# Estimate how trusted a node will be for the target context
estimate.trust <- function(weights, context.target, context.weighted, eta, impact.factor, trust.current)
{
    if (trust.current <= 0) {
        return (
            max(
                -1,
                trust.current *
                prod(
                    2 - eta[[3]] **
                    delta(weights, context.target, context.weighted, impact.factor, context.target > context.weighted)
                ) *
                prod(
                    eta[[4]] **
                    delta(weights, context.target, context.weighted, impact.factor, context.target < context.weighted)
                )
            )
        )
    }
    return (
        min(
            1,
            trust.current *
            prod(
                eta[[1]] **
                delta(weights, context.target, context.weighted, impact.factor, context.target > context.weighted)
            ) *
            prod(
                2 - eta[[2]] **
                delta(weights, context.target, context.weighted, impact.factor, context.target < context.weighted)
            )
        )
    )
}


# A function used within the trust estimation
delta <- function(weights, context.target, context.weighted, impact.factor, cond)
{
    return (
        (weights[cond] * abs(context.target[cond] - context.weighted[cond])) / impact.factor
    )
}



# Calculate a weighted trust
weighted.trust <- function(trust.estimate, trust, distrust, unknown, alpha, beta, gamma)
{
    prob.trust <- compute.probability(trust, distrust, unknown)
    prob.distrust <- compute.probability(distrust, trust, unknown)
    prob.unknown <- compute.probability(unknown, trust, distrust)

    return (
        `if`(
            prob.trust > max(prob.unknown, prob.distrust),
            alpha,
            `if`(
                prob.unknown >= max(prob.trust, prob.distrust) && prob.unknown != prob.distrust,
                beta,
                gamma
            )
        ) * trust.estimate)
}



# Calculate the direct trust
direct.trust <- function(trusts, weights, context.target, context.weighted, eta, delta)
{
    return (sum(omega(weights, context.weighted, context.target, eta, delta) * trusts))
}



# Calculate the indirect trust
indirect.trust <- function(trusts, reputations, weights, context.target, context.weighted, context.cached, eta, delta)
{
    omega.weighted <- omega(weights, context.weighted, context.target, eta, delta)
    omega.cached <- omega(weights, context.cached, context.target, eta, delta)
    return ((sum(omega.weighted * omega.cached * reputations * trusts)) / sum(omega.weighted))
}


# A function used within the indirect and direct trust calculations
omega <- function(weights, context.weighted, context.target, eta, delta)
{
    return (eta ** (context.distance(weights, context.weighted, context.target) / delta))
}


# Update the amounts of transactions that the service provider has performed
# and classifying it
update.performance <- function(service.provider, trust.direct0, trust.direct1, trust.indirect0, trust.indirect1,
                               weights, context.indirect0, context.indirect1, context.direct0, context.direct1,
                               eta, delta, trust.threshold)
{
    trend.indirect <- trend.of.trust(trust.indirect0, trust.indirect1, weights, context.indirect0, context.indirect1, eta, delta)
    trend.direct <- trend.of.trust(trust.direct0, trust.direct1, weights, context.direct0, context.direct1, eta, delta)
    trends.diff <- abs(trend.direct - trend.indirect)
    trends.max <- max(abs(trend.direct), abs(trend.indirect))

    if (trends.diff >= 0 && trends.diff < trends.max) {
        service.provider$trust.increment()
    } else if (trends.diff >= trends.max && trends.diff <= trust.threshold) {
        service.provider$unknown.increment()
    } else {
        service.provider$distrust.increment()
    }
}


# Calculate the expected value of change in the trust
trend.of.trust <- function(trust0, trust1, weights, context0, context1, eta, delta)
{
    return (trust1 - eta ** (context.distance(weights, context1, context0) / delta) * trust0)
}


# Calculate a new reputation value for a service provider
reputation.combination <- function(weights, context.old, context.target,
                                   reputation.old, reputation, theta, eta,
                                   delta, rho)
{
    context.new <- weighted.avg.context(theta, c(context.old, context.target))  # apply? for each of the 4
    omega.new.old <- omega(weights, context.new, context.old, eta, delta)
    omega.new.target <- omega(weights, context.new, context.target, eta, delta)
    return (
        `if`(
            reputation.old * reputation > 0,
            omega.new.old * reputation.old + omega.new.target *
                rho ** (omega.new.old * abs(reputation.old)) * reputation,
            omega.new.old * reputation.old + omega.new.target *
                rho ** (1 - omega.new.old * abs(reputation.old)) * reputation
        )
    )
}
