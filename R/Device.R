#' @include Params.R
#' @include Functions.R
#' @include TrustModel.R
#' @include ServiceProvider.R
#' @include Observation.R
#' @include Normalizers.R
#' @include Field.R

Device <- setRefClass(
    "Device",

    fields = list(
        id = "numeric",
        contacts = "numeric",
        location = "numeric",
        current.goal = "numeric",
        capability = "numeric",
        velocity = "numeric",
        trust = "numeric",
        distrust = "numeric",
        unknown = "numeric",
        domain = "numeric",
        reputations = "numeric",
        reputations.cached = "numeric",
        recommendations = "list",
        recommendations.cached = "list",
        service.provider = "ServiceProvider",
        last.rec.time = "numeric",
        time.last.moved = "numeric",
        estimated.trusts = "numeric",
        map = "list"
    ),

    methods = list(
        initialize = function(id, map, loc=round(runif(2, min=1, max=map.size))) {
            if (!is.null(map)) {
                map.size <- map$shape()
                map <<- list(map)
                location <<- loc
                if (round(runif(1)) == 1) {
                    domain <<- AIR
                } else {
                    domain <<- map$get.tile(location)[[1]]$terrain
                }
                new.goal()
            } else {
                map.size <- c(params$map.width, params$map.height)
                location <<- loc
                map <<- list()
                domain <<- sample(c(AIR, LAND, WATER), 1)
            }
            id <<- id
            set.trusts()
            contacts <<- sample(
                1:params$number.nodes,
                round(runif(1, min=1, max=min(params$max.number.contacts, params$number.nodes)))
            )
            if (!is.null(map)) {
                map$get.tile(location)[[1]]$add.device(.self)
                for (signal in map$get.tile(location)[[1]]$signals) {
                    signal$connect(.self)
                }
            }
            velocity <<- runif(1, min=0, max=params$max.velocity)
            capability <<- runif(1, min=1, max=params$max.capability)
            reputations <<- rep(params$init.reputation, params$number.nodes)
            reputations.cached <<- rep(params$init.reputation, params$number.nodes)
            recommendations <<- init.recommendations()
            recommendations.cached <<- init.recommendations()
            last.rec.time <<- -Inf
            time.last.moved <<- params$time.now
            estimated.trusts <<- c(params$trust.new.contact)
        },

        init.recommendations = function() {
            return(
                sapply(
                    1:params$number.nodes,
                    function(i) {
                        Observation(
                            rep(0, length(params$context.weights)),
                            params$trust.new.contact,
                            i,
                            FALSE
                        )
                    }
                )
            )
        },

        set.trusts = function() {
            "Set up the trusts for the service providers in the network"
            trust <<- rep(0, params$number.service.providers)
            distrust <<- rep(0, params$number.service.providers)
            unknown <<- rep(0, params$number.service.providers)
        },

        new.goal = function() {
            while (all(current.goal == location) ||
                   (domain == WATER && map[[1]]$get.tile(current.goal)[[1]]$terrain)) {
                current.goal <<- round(runif(2, min=1, max=map[[1]]$shape()))
            }
        },

        trust.increment = function(sp.id) {
            trust[[sp.id]] <<- trust[[sp.id]] + 1
        },

        distrust.increment = function(sp.id) {
            distrust[[sp.id]] <<- distrust[[sp.id]] + 1
        },

        unknown.increment = function(sp.id) {
           unknown[[sp.id]] <<- unknown[[sp.id]] + 1
        },

        recieve.observation = function(observation) {
            "Receive a recommendation from the sender"
            if (should.consider.rec(observation)) {
                recommendations[[observation$id.sender]] <<- observation
            }
        },

        should.consider.rec = function(rec) {
            return (rec$trust > (params$trust.rep.threshold - params$trust.rep.adj.range) ||
                    acceptable.rec(recommendations[[rec$id.sender]], rec))
        },

        move = function() {
            "Move towards the current goal"
            time.change <- params$time.now - time.last.moved
            old.signals <- get.signals()
            disconnect.all()
            movement.amount <- round(velocity * time.change)
            movement <- `if`(movement.amount > 0, 1:movement.amount, NULL)
            for (m in movement) {
                best.weight <- Inf
                best.loc <- NA
                best.tile <- NA
                for (i in (location[[1]] - 1):(location[[1]] + 1)) {
                    for (j in (location[[2]] - 1):(location[[2]] + 1)) {
                        loc <- c(i, j)
                        tile <- map[[1]]$get.tile(loc)
                        if (!all(loc == location) && length(tile)) {
                            tile <- tile[[1]]
                            cost <- `if`(
                                domain == AIR,
                                1,
                                `if`(
                                    domain == tile$terrain,
                                    1,
                                    2
                                )
                            )
                            weight <- cost + euc.dist(loc, current.goal)
                            if (weight < best.weight) {
                                best.weight <- weight
                                best.loc <- loc
                                best.tile <- tile
                            }
                        }
                    }
                }
                if (!all(is.na(best.loc))) {
                    map[[1]]$get.tile(location)[[1]]$rm.device(id)
                    location <<- best.loc
                    best.tile$add.device(.self)
                }
            }
            connect.all()
            retabulate.all(old.signals)
            velocity <<- min(max(0, velocity + rnorm(1)), params$max.velocity)
            if (all(location == current.goal)) {
                new.goal()
            }
            time.last.moved <<- params$time.now
        },

        disconnect.all = function() {
            for (signal in map[[1]]$get.tile(location)[[1]]$signals) {
                signal$disconnect(.self)
            }
        },

        connect.all = function() {
            for (signal in map[[1]]$get.tile(location)[[1]]$signals) {
                signal$connect(.self)
            }
        },

        retabulate.all = function(old.signals) {
            if (has.signal()) {
                check.signals <- get.signals()
            } else {
                check.signals <- old.signals
            }
            for (signal in check.signals) {
                signal$retabulate(.self)
            }
            for (signal in check.signals) {
                signal$finish.update()
            }
        },

        has.signal = function() {
            return (length(map[[1]]$get.tile(location)[[1]]$signals) > 0)
        },

        get.signals = function() {
            return (map[[1]]$get.tile(location)[[1]]$signals)
        },

        transaction = function(devices) {
            "Perform a transaction with a service provider"
            sp.id <- 1
            update.recommendations()
            context.target <- get.target.context()
            normalized.c.target <- normalize(context.target)
            update.performance(sp.id)
            reputation.update()
            cur.obs <- find.direct.trust(sp.id, context.target, normalized.c.target)
            if (abs(cur.obs$trust) <= params$trust.rep.adj.range) {
                cur.obs <- find.indirect.trust(sp.id, context.target, normalized.c.target)
            }
            if (cur.obs$trust > params$trust.rep.threshold - params$trust.rep.adj.range) {
                service.provider$provide.service()
            }
            prev.est.trust <- tail(estimated.trusts, 1)
            for (i in length(estimated.trusts):params$time.now) {
                if (i < params$time.now) {
                    estimated.trusts[[i]] <<- prev.est.trust
                } else {
                    estimated.trusts[[i]] <<- cur.obs$trust
                }
            }
            recieve.observation(cur.obs)
            if (last.rec.time < params$time.now) {
                # send resulting observation to all contacts
                emit.observation(cur.obs, devices)
                last.rec.time <<- params$time.now
            }
        },

        update.recommendations = function() {
            "Update the times stored in all of the recommendations"
            if (last.rec.time < params$time.now) {
                for (i in 1:length(recommendations)) {
                    recommendations[[i]]$update.time()
                    recommendations.cached[[i]]$update.time()
                }
            }
        },

        find.weighted.context = function(contexts) {
            "Find the weighted context of the given vector of contexts"
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
        },

        get.target.context = function() {
            "Get the current target context"
            return (
                c(
                    params$time.now,
                    capability,
                    euc.dist(location, service.provider$location),
                    velocity
                )
            )
        },

        find.direct.trust = function(sp.id, context.target, normalized.c.target) {
            trust.evaled <- compute.trust(trust[[sp.id]], distrust[[sp.id]], unknown[[sp.id]])
            context.weighted <- find.weighted.context(
                c(recommendations[[id]]$context, recommendations.cached[[id]]$context, normalized.c.target)
            )
            dir.trust <- direct.trust(
                c(
                    recommendations.cached[[id]]$trust,
                    recommendations[[id]]$trust,
                    trust.evaled
                ),
                c(recommendations.cached[[id]]$context, recommendations[[id]]$context, normalized.c.target),
                context.weighted
            )
            return (
                Observation(
                    context.weighted,
                    estimate.trust(
                        normalized.c.target,
                        context.weighted,
                        weighted.trust(dir.trust, trust[[sp.id]], distrust[[sp.id]], unknown[[sp.id]])
                    ),
                    id
                )
            )
        },

        find.indirect.trust = function(sp.id, context.target, normalized.c.target) {
            indirect.contexts <- sapply(
                setdiff(1:length(recommendations), id),
                function(i) { recommendations[[i]]$context }
            )
            indirect.contexts.cached <- sapply(
                setdiff(1:length(recommendations), id),
                function(i) { recommendations.cached[[i]]$context }
            )
            context.weighted <- find.weighted.context(
                c(indirect.contexts.cached, indirect.contexts, normalized.c.target)
            )
            ind.trust <- indirect.trust(
                sapply(
                    setdiff(1:length(recommendations), id),
                    function(i) { recommendations[[i]]$trust }
                ),
                reputations[setdiff(1:length(reputations), id)],
                context.weighted,
                indirect.contexts,
                indirect.contexts.cached
            )
            return (
                Observation(
                    context.weighted,
                    estimate.trust(
                        normalized.c.target,
                        context.weighted,
                        weighted.trust(ind.trust, trust[[sp.id]], distrust[[sp.id]], unknown[[sp.id]])
                    ),
                    id
                )
            )
        },

        estimate.and.weigh.trust = function(c.target, c.weighted, trust.eval, sp.id) {
            "Get the weighted trust estimate for the given context"
            return (
                weighted.trust(
                    estimate.trust(c.target, c.weighted, trust.eval),
                    trust[[sp.id]],
                    distrust[[sp.id]],
                    unknown[[sp.id]]
                )
            )
        },

        update.performance = function(sp.id) {
            "Update the amounts of transactions that the service provider has
            performed and classify it"
            trend.direct <- trend.of.trust(
                recommendations.cached[[id]]$trust,
                recommendations[[id]]$trust,
                recommendations.cached[[id]]$context,
                recommendations[[id]]$context
            )
            sapply(1:length(recommendations),
                function(i) {
                    if (recommendations.cached[[i]]$valid) {
                        trend.indirect <- trend.of.trust(
                            recommendations.cached[[i]]$trust,
                            recommendations[[i]]$trust,
                            recommendations.cached[[i]]$context,
                            recommendations[[i]]$context
                        )
                        trends.diff <- abs(trend.direct - trend.indirect)
                        trends.max <- max(abs(trend.direct), abs(trend.indirect))

                        if (trends.diff >= 0 && trends.diff < trends.max) {
                            trust.increment(sp.id)
                        } else if (trends.diff >= trends.max && trends.diff <= params$trust.rep.threshold) {
                            unknown.increment(sp.id)
                        } else {
                            distrust.increment(sp.id)
                        }
                    }
                }
            )
        },

        reputation.update = function() {
            "Update the reputations of the nodes"
            sapply(
                1:length(reputations),
                function(i) {
                    if (recommendations.cached[[i]]$valid) {
                        reputations.cached[[i]] <<- reputations[[i]]
                        reputations[[i]] <<- reputation.combination(
                            recommendations.cached[[i]]$context,
                            recommendations[[i]]$context,
                            reputations.cached[[i]],
                            reputations[[i]]
                        )
                    }
                    recommendations.cached[[i]] <<- recommendations[[i]]
                }
            )
        },

        emit.observation = function(observation, devices) {
            "send observation to all contacts"
            for (contact in contacts) {
                connection.data <- communicate(contact)
                if (connection.data[[1]] < Inf) {
                    # routed communication
                    connection.data[[2]]$recieve.observation(observation)
                } else if (euc.dist(devices[[contact]]$location, location) <= params$dev.signal.radius) {
                    # direct communication
                    devices[[contact]]$recieve.observation(observation)
                }
            }
        },

        communicate = function(contact.id) {
            "Communicate with a random contact"
            this.tile <- map[[1]]$get.tile(location)[[1]]
            best.signal <- 1
            for (i in 1:length(this.tile$signals)) {
                if (this.tile$signals[[i]]$table$hops[[contact.id]] <=
                    this.tile$signals[[best.signal]]$table$hops[[contact.id]]) {
                    best.signal <- i
                }
            }
            if (this.tile$signals[[best.signal]]$table$hops[[contact.id]] < Inf) {
                other.device <- this.tile$signals[[best.signal]]$find.device(contact.id)
            } else {
                other.device <- NULL
            }
            return (
                list(
                    this.tile$signals[[best.signal]]$table$hops[[contact.id]],
                    other.device
                )
            )
        }
    )
)
