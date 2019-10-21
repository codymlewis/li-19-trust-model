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
weighted.avg.context <- function(contexts)
{
    context.latest <- tail(contexts, 1)
    factor.forget <- params$theta.i**abs(context.latest - head(contexts, -1))
    return ((context.latest + sum(factor.forget * head(contexts, -1))) / (1 + sum(factor.forget)))
}


# Take a vector of the contexts and return the weighted average
find.weighted.context <- function(contexts) {
    return (
        apply(
            matrix(
                contexts,
                nrow=length(params$context.weights)
            ),
            1,
            weighted.avg.context
        )
    )
}


# Find the distance between the target context, and the weighted context
context.distance <- function(context.target, context.weighted)
{
    return (
        sqrt(
            sum(
                params$context.weights * (context.target - context.weighted)**2
            )
        )
    )
}


# Estimate how trusted a node will be for the target context
estimate.trust <- function(context.target, context.weighted, trust.current)
{
    if (trust.current <= 0) {
        return (
            max(
                -1,
                trust.current *
                prod(
                    2 - params$eta[[4]] **
                    delta(context.target, context.weighted, context.target > context.weighted)
                ) *
                prod(
                    params$eta[[5]] **
                    delta(context.target, context.weighted, context.target < context.weighted)
                )
            )
        )
    }
    return (
        min(
            1,
            trust.current *
            prod(
                params$eta[[2]] **
                delta(context.target, context.weighted, context.target > context.weighted)
            ) *
            prod(
                2 - params$eta[[3]] **
                delta(context.target, context.weighted, context.target < context.weighted)
            )
        )
    )
}


# A function used within the trust estimation
delta <- function(context.target, context.weighted, cond)
{
    return (
        (params$context.weights[cond] *
         abs(context.target[cond] - context.weighted[cond])) /
        params$impact.factor
    )
}


# Calculate a weighted trust
weighted.trust <- function(trust.estimate, trust, distrust, unknown)
{
    prob.trust <- compute.probability(trust, distrust, unknown)
    prob.distrust <- compute.probability(distrust, trust, unknown)
    prob.unknown <- compute.probability(unknown, trust, distrust)

    return (
        `if`(
            prob.trust > max(prob.unknown, prob.distrust),
            params$alpha,
            `if`(
                prob.unknown >= max(prob.trust, prob.distrust) && prob.unknown != prob.distrust,
                params$beta,
                params$gamma
            )
        ) * trust.estimate)
}


# Calculate the direct trust
direct.trust <- function(trusts, context.target, context.weighted)
{
    return (sum(omega(context.weighted, context.target) * trusts))
}


# Calculate the indirect trust
indirect.trust <- function(trusts, reputations, contexts, context.weighted, context.cached)
{
    omega.weighted <- omega(context.weighted, contexts)
    omega.cached <- omega(context.cached, contexts)
    return (
        sum(omega.weighted * omega.cached * reputations * trusts) /
            sum(omega.weighted)
    )
}


# A function used within the indirect and direct trust calculations
omega <- function(context.weighted, context.target)
{
    return (
        params$eta[[1]] ** (
            apply(
                matrix(context.target, ncol=length(context.weighted), byrow=T),
                1,
                function(c) {
                    return (context.distance(context.weighted, c))
                }
            ) / params$delta
        )
    )
}


# Calculate the expected value of change in the trust
trend.of.trust <- function(trust0, trust1, context0, context1)
{
    return (
        trust1 -
            params$eta[[1]] ** (context.distance(context1, context0) / params$delta)
        * trust0
    )
}


# Calculate a new reputation value for a service provider
reputation.combination <- function(context.old, context.target, reputation.old, reputation)
{
    context.new <- find.weighted.context(c(context.old, context.target))
    omega.new.old <- omega(context.new, context.old)
    omega.new.target <- omega(context.new, context.target)
    return (
        `if`(
            reputation.old * reputation > 0,
            omega.new.old * reputation.old + omega.new.target *
                params$rho ** (omega.new.old * abs(reputation.old)) * reputation,
            omega.new.old * reputation.old + omega.new.target *
                params$rho ** (1 - omega.new.old * abs(reputation.old)) * reputation
        )
    )
}


acceptable.rec <- function(old.rec, new.rec)
{
    return (
        abs(
            old.rec$trust *
                params$eta[[1]]**(
                    context.distance(new.rec$context, old.rec$context) /
                        params$delta
                )
        ) <
        params$trust.rep.threshold + params$trust.rep.adj.range
    )
}
