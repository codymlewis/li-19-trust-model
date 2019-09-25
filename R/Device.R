#' @include Params.R
#' @include Functions.R
#' @include TrustModel.R
#' @include ServiceProvider.R

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
        indirect.observations = "list",
        service.provider = "ServiceProvider"
    ),

    methods = list(
        initialize = function(id, map) {
            id <<- id
            set.trusts()
            contacts <<- sample(1:Params$number.nodes, round(runif(1, min=1, max=100)))
            location <<- round(runif(2, min=0, max=(map$size() - 1)))
            map$get.tile(location)$add.device(id, .self)
            velocity <<- runif(1, min=0, max=Params$max.velocity)
            current.goal <<- round(runif(2, min=0, max=(map.size - 1)))
            if (round(runif(1)) == 1) {
                domain <<- AIR
            } else {
                domain <<- map[[location[[1]]]][[location[[2]]]]$terrain
            }
            contexts <<- c(
                list(),
                rep(
                    list(rep(0, length(Params$context.weights))),
                    Params$number.nodes
                )
            )
            contexts.cached <<- c(
                list(),
                rep(
                    list(rep(0, length(Params$context.weights))),
                    Params$number.nodes
                )
            )
            trust.evals <<- rep(Params$trust.new.contact, Params$number.nodes)
            reputations <<- rep(Params$init.reputation, Params$number.nodes)
            reputations.cached <<- rep(Params$init.reputation, Params$number.nodes)
            indirect.observations <<- list()
        },

        set.trusts = function() {
            "Set up the trusts for the service providers in the network"
            trust <<- rep(0, Params$number.service.provider)
            distrust <<- rep(0, Params$number.service.provider)
            unknown <<- rep(0, Params$number.service.provider)
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
            indirect.observations[[length(indirect.observations)]] <<- observation
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
            context.target <- c()
            update.performance()
            reputation.update()
            cur.trust <- direct.trust(c(trusts.cached, trusts), context.target, contexts)
            if (abs(cur.trust) <= Params$trust.rep.adj.range) {
                # cur.trust <- indirect.trust()
            }
            if (cur.trust > Params$trust.rep.threshold - Params$trust.rep.adj.range) {
                service.provider$provide.service()
            }
            # send resulting observation to all contacts
            emit.observation()
        },

        # Update the amounts of transactions that the service provider has performed
        # and classifying it
        update.performance = function() {
            trend.direct <- trend.of.trust(
                trust.evals.cached, trust.evals, contexts.cached, contexts
            )
            sapply(1:length(indirect.observations),
                function(i) {
                    trend.indirect <- trend.of.trust(
                        trust.indirect0, trust.indirect1, context.indirect0, context.indirect1
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

        emit.observation = function() {
            # send observation to all contacts
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
