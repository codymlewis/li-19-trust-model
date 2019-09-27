#' @include Params.R
#' @include Functions.R
#' @include TrustModel.R
#' @include ServiceProvider.R
#' @include Observation.R
#' @include Normalize.R

Device <- setRefClass(
    "Device",

    fields = list(
        id = "numeric",
        contacts = "numeric",
        location = "numeric",
        current.goal = "numeric",
        time = "numeric",
        capability = "numeric",
        velocity = "numeric",
        trust = "numeric",
        distrust = "numeric",
        unknown = "numeric",
        domain = "numeric",
        contexts = "list",
        contexts.cached = "list",
        trust.evals = "numeric",
        reputations = "numeric",
        reputations.cached = "numeric",
        recommendations = "list",
        recommendations.cached = "list",
        service.provider = "ServiceProvider",
        last.rec.time = "numeric"
    ),

    methods = list(
        initialize = function(id, map) {
            if (is.null(map)) {
                map.size <- c(Params$map.width, Params$map.height)
            }
            id <<- id
            set.trusts()
            contacts <<- sample(
                1:Params$number.nodes,
                round(runif(1, min=1, max=min(Params$max.number.contacts, Params$number.nodes)))
            )
            location <<- round(runif(2, min=1, max=map.size))
            if (!is.null(map)) {
                map$get.tile(location)$add.device(id, .self)
            }
            velocity <<- runif(1, min=0, max=Params$max.velocity)
            current.goal <<- round(runif(2, min=1, max=map.size))
            if (!is.null(map)) {
                if (round(runif(1)) == 1) {
                    domain <<- AIR
                } else {
                    domain <<- map[[location[[1]]]][[location[[2]]]]$terrain
                }
            } else {
                domain <<- sample(c(AIR, LAND, WATER), 1)
            }
            contexts <<- init.contexts()
            contexts.cached <<- init.contexts()
            trust.evals <<- rep(Params$trust.new.contact, Params$number.nodes)
            reputations <<- rep(Params$init.reputation, Params$number.nodes)
            reputations.cached <<- rep(Params$init.reputation, Params$number.nodes)
            recommendations <<- init.recommendations()
            recommendations.cached <<- init.recommendations()
            last.rec.time <<- -Inf
        },

        init.contexts = function() {
            return(
                c(
                    list(),
                    rep(
                        list(rep(0, length(Params$context.weights))),
                        Params$number.nodes
                    )
                )
            )
        },

        init.recommendations = function() {
            return(
                sapply(
                    1:Params$number.nodes,
                    function(i) {
                        Observation(
                            rep(0, length(Params$context.weights)),
                            Params$trust.new.contact,
                            i
                        )
                    }
                )
            )
        },

        set.trusts = function() {
            "Set up the trusts for the service providers in the network"
            trust <<- rep(0, Params$number.service.providers)
            distrust <<- rep(0, Params$number.service.providers)
            unknown <<- rep(0, Params$number.service.providers)
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

        recieve.observation = function(sender.id, observation) {
            "Receive a recommendation from the sender"
            if (!is.null(recommendations[sender.id][[1]])) {
                recommendations.cached[[sender.id]] <<- recommendations[[sender.id]]
            }
            recommendations[[sender.id]] <<- observation
        },

        move = function(time.change, map) {
            "Move towards the current goal"
            for (signal in map$get.tile(location)$signals) {
                signal$disconnect(.self)
            }
            movement.amount <- round(velocity * time.change)
            movement <- `if`(movement.amount > 0, 1:movement.amount, NULL)
            for (m in movement) {
                best.weight <- Inf
                best.loc <- NA
                best.tile <- NA
                for (i in location[[1]] - 1:location[[1]] + 1) {
                    for (j in location[[2]] - 1:location[[2]] + 1) {
                        tile <- map$get.tile(c(i, j))
                        loc <- c(i, j)
                        if (!(all(loc %in% location)) && !is.na(tile)) {
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
                map$get.tile(location)$rm.device(id)
                location <<- best.loc
                best.tile$add.device(id, .self)
            }
            for (signal in map$get.tile(location)$signals) {
                signal$connect(.self)
            }
            velocity <<- min(max(0, velocity + rnorm(1)), Params$max.velocity)
            if (all(location %in% current.goal)) {
                current.goal <<- round(runif(2, min=0, max=(map$size() - 1)))
            }
        },

        transaction = function() {
            "Perform a transaction with a service provider"
            context.target <- c(
                Params$time.now,
                capability,
                euc.dist(location, service.provider$location),
                velocity
            )
            update.performance()
            reputation.update()
            cur.trust <- direct.trust(
                c(trusts.cached, trusts), normalize(context.target), context.prev
            )
            if (abs(cur.trust) <= Params$trust.rep.adj.range) {
                # cur.trust <- indirect.trust()
            }
            if (cur.trust > Params$trust.rep.threshold - Params$trust.rep.adj.range) {
                service.provider$provide.service()
            }
            if (last.rec.time < Params$time.now) {
                # send resulting observation to all contacts
                emit.observation(Observation(context.target, cur.trust, id))
                last.rec.time <- Params$time.now
            }
        },

        update.performance = function() {
            "Update the amounts of transactions that the service provider has
            performed and classify it"
            trend.direct <- trend.of.trust(
                trust.evals.cached, trust.evals, contexts.cached, contexts
            )
            sapply(1:length(recommendations),
                function(i) {
                    trend.indirect <- trend.of.trust(
                        recommendations.cached[[i]]$trust,
                        recommendations[[i]]$trust,
                        recommendations.cached[[i]]$context,
                        recommendations[[i]]$contexts
                    )
                    trends.diff <- abs(trend.direct - trend.indirect)
                    trends.max <- max(abs(trend.direct), abs(trend.indirect))

                    if (trends.diff >= 0 && trends.diff < trends.max) {
                        service.provider$trust.increment()
                    } else if (trends.diff >= trends.max && trends.diff <= Params$trust.rep.threshold) {
                        service.provider$unknown.increment()
                    } else {
                        service.provider$distrust.increment()
                    }
                }
            )
        },

        reputation.update = function() {
            "Update the reputations of the nodes"
            reputations.new <- sapply(
                1:length(reputations),
                function(i) {
                    reputation.combination(
                        contexts.cached[[i]],
                        contexts[[i]],
                        reputations.cached[[i]],
                        reputations[[i]]
                    )
                }
            )
            reputations.cached <<- reputations
            reputations <<- reputations.new
        },

        emit.observation = function(observation) {
            "send observation to all contacts"
            for (contact in contacts) {
                contact$recieve.observation(id, observation)
            }
        },

        communicate = function(map) {
            "Communicate with a random contact"
            id.other <- sample(contacts, 1)
            this.tile <- map$get.tile(location)
            best.signal <- 1
            for (i in 1:length(this.tile$signals)) {
                if (this.tile$signals[[i]]$table$hops[[id.other]] <=
                    this.tile$signals[[best.signal]]$hops[[id.other]]) {
                    best.signal <- i
                }
            }
            other.device <- this.tile$signals[[best.signal]]$find.device(id.other)
            return (
                list(
                    this.tile$signals[[best.signal]]$hops[[id.other]],
                    other.device
                )
            )
        }
    )
)
