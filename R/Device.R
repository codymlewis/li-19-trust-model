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
        map = "list",
        trust.evals = "numeric",
        all.contexts = "numeric"
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
            trust.evals <<- params$trust.new.contact
            all.contexts <<- get.target.context()
        },

        init.recommendations = function() {
            "Create default observations for each of the nodes"
            return(
                sapply(
                    1:params$number.nodes,
                    function(i) {
                        Observation(
                            rep(0, length(params$context.weights)),
                            params$time.now,
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
            trust <<- rep(0, params$number.nodes)
            distrust <<- rep(0, params$number.nodes)
            unknown <<- rep(0, params$number.nodes)
        },

        reset.trusts = function() {
            "Reset the trusts of each of the nodes within the network"
            trust[setdiff(1:params$number.nodes, id)] <<- rep(0, params$number.nodes - 1)
            distrust[setdiff(1:params$number.nodes, id)] <<- rep(0, params$number.nodes - 1)
            unknown[setdiff(1:params$number.nodes, id)] <<- rep(0, params$number.nodes - 1)
        },

        new.goal = function() {
            "Find a new location to head towards"
            while (all(current.goal == location) ||
                   (domain == WATER && map[[1]]$get.tile(current.goal)[[1]]$terrain != WATER)) {
                current.goal <<- round(runif(2, min=1, max=map[[1]]$shape()))
            }
        },

        trust.increment = function(contact.id) {
            "Increment the trust count of the service provider"
            trust[[contact.id]] <<- trust[[contact.id]] + 1
        },

        distrust.increment = function(contact.id) {
            "Increment the distrust count of the service provider"
            distrust[[contact.id]] <<- distrust[[contact.id]] + 1
        },

        unknown.increment = function(contact.id) {
            "Increment the unknown count of the service provider"
            unknown[[contact.id]] <<- unknown[[contact.id]] + 1
        },

        recieve.observation = function(observation) {
            "Receive a recommendation from the sender"
            if (should.consider.rec(observation)) {
                recommendations[[observation$id.sender]] <<- observation
            }
        },

        should.consider.rec = function(rec) {
            "Check whether the recommendation should be considered"
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
            "Disconnect from all base stations that this is currently connected to"
            for (signal in map[[1]]$get.tile(location)[[1]]$signals) {
                signal$disconnect(.self)
            }
        },

        connect.all = function() {
            "Connect to all base stations currently in range of this"
            for (signal in map[[1]]$get.tile(location)[[1]]$signals) {
                signal$connect(.self)
            }
        },

        retabulate.all = function(old.signals) {
            "After changing from being in one set of signals to another, make
            them recalculate their routing tables"
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
            "Check whether this has signal"
            return (length(map[[1]]$get.tile(location)[[1]]$signals) > 0)
        },

        get.signals = function() {
            "Get the list of signals in range of this"
            return (map[[1]]$get.tile(location)[[1]]$signals)
        },

        transaction = function(devices) {
            "Perform a transaction with a service provider"
            update.recommendations()
            context.target <- get.target.context()
            normalized.c.target <- normalize(context.target)
            update.performance()
            reputation.update()
            dir.trust <- find.direct.trust(context.target, normalized.c.target)
            # print(sprintf("direct trust: %f", dir.trust$trust.est))
            used.trust <- `if`(
                abs(dir.trust$trust.est) <= params$trust.rep.threshold + params$trust.rep.adj.range,
                find.indirect.trust(context.target, normalized.c.target),
                dir.trust$trust.est
            )
            # print(sprintf("indirect trust: %f", find.indirect.trust(context.target, normalized.c.target)))
            # print(sprintf("used trust: %f", used.trust))
            if (used.trust > params$trust.rep.threshold - params$trust.rep.adj.range) {
                # if (used.trust > params$trust.rep.threshold + params$trust.rep.adj.range) {
                #     trust.increment(id)
                # } else {
                #     unknown.increment(id)
                # }
                service.provider$provide.service()
            # } else {
            #     distrust.increment(id)
            }
            trust.increment(id)
            prev.est.trust <- tail(estimated.trusts, 1)
            for (i in length(estimated.trusts):params$time.now) {
                if (i < params$time.now) {
                    estimated.trusts[[i]] <<- prev.est.trust
                } else {
                    estimated.trusts[[i]] <<- used.trust
                }
            }
            recieve.observation(dir.trust$obs)
            if (last.rec.time < params$time.now) {
                # send resulting observation to all contacts
                emit.observation(dir.trust$obs, devices)
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

        find.direct.trust = function(context.target, normalized.c.target) {
            "Find the direct trust of the service provider"
            print(sprintf("t: %d, d: %d, u: %d", trust[[id]], distrust[[id]], unknown[[id]]))
            trust.evaled <- weighted.trust(
                compute.trust(trust[[id]], distrust[[id]], unknown[[id]]),
                trust[[id]],
                distrust[[id]],
                unknown[[id]]
            )
            trust.evals <<- c(trust.evals, trust.evaled)
            all.contexts <<- c(all.contexts, context.target)
            contexts <- c(
                recommendations.cached[[id]]$get.context(),
                recommendations[[id]]$get.context()
            )
            # print("contexts")
            # print(contexts)
            # context.weighted <- find.weighted.context(contexts)
            # print("weighted contexts")
            # print(context.weighted)
            normed.contexts <- as.vector(
                apply(matrix(all.contexts, ncol=length(context.target), byrow=TRUE), 1, normalize)
            )
            # print(normed.contexts)
            context.weighted <- find.weighted.context(normed.contexts)
            # print(c(contexts, context.weighted))
            # dir.trust <- direct.trust(
            #     c(
            #         recommendations.cached[[id]]$trust,
            #         recommendations[[id]]$trust,
            #         trust.evaled
            #     ),
            #     c(contexts, context.weighted),
            #     context.weighted
            # )
            dir.trust <- direct.trust(
                trust.evals,
                c(normed.contexts, context.weighted),
                context.weighted
            )
            print(sprintf("direct trust evaled: %f", trust.evaled))
            print(sprintf("combined direct trust: %f", dir.trust))
            print(sprintf("estimated trust: %f", estimate.trust(normalized.c.target, context.weighted, dir.trust)))
            # print("context weighted")
            # print(context.weighted)
            # print("new context weighted")
            # print(find.weighted.context(c(contexts, normalized.c.target)))
            return (
                list(
                    obs=Observation(
                        find.weighted.context(c(contexts, normalized.c.target)),
                        params$time.now,
                        trust.evaled,
                        # dir.trust,
                        id
                    ),
                    trust.est=estimate.trust(
                        normalized.c.target,
                        context.weighted,
                        dir.trust
                    )
                )
            )
        },
        # TODO: fix initial observation issue

        find.indirect.trust = function(context.target, normalized.c.target) {
            "Find the indirect trust of the service providerc"
            indirect.contexts <- sapply(
                setdiff(1:length(recommendations), id),
                function(i) {
                    `if`(reputations[[i]] >= 0, recommendations[[i]]$get.context(), NULL)
                }
            )
            indirect.contexts.cached <- sapply(
                setdiff(1:length(recommendations), id),
                function(i) {
                    `if`(reputations[[i]] >= 0, recommendations.cached[[i]]$get.context(), NULL)
                }
            )
            context.weighted <- find.weighted.context(
                # c(indirect.contexts.cached, indirect.contexts)
                indirect.contexts
            )
            # print(sprintf("cwl: %d", length(context.weighted)))
            used.reps <- reputations[setdiff(1:length(reputations), id)]
            ind.trust <- indirect.trust(
                sapply(
                    setdiff(1:length(recommendations), id),
                    function(i) {
                        `if`(reputations[[i]] >= 0, recommendations[[i]]$trust, NULL)
                    }
                ),
                used.reps[used.reps >= 0],
                context.weighted,
                indirect.contexts,
                indirect.contexts.cached
            )
            # print(sprintf("indirect trust combined: %f", ind.trust))
            return (
                estimate.trust(
                    normalized.c.target,
                    context.weighted,
                    ind.trust
                )
            )
        },

        update.performance = function() {
            "Update the amounts of transactions that the service provider has
            performed and classify it"
            trend.direct <- trend.of.trust(
                recommendations.cached[[id]]$trust,
                recommendations[[id]]$trust,
                recommendations.cached[[id]]$context,
                recommendations[[id]]$context
            )
            # print(sprintf("trend of trust: %f", trend.direct))
            sapply(1:length(recommendations),
                function(i) {
                    if (recommendations.cached[[i]]$valid) {
                        trend.indirect <- trend.of.trust(
                            recommendations.cached[[i]]$trust,
                            recommendations[[i]]$trust,
                            recommendations.cached[[i]]$context,
                            recommendations[[i]]$context
                        )
                        # print(sprintf("direct trend of trust: %f", trend.direct))
                        # print(sprintf("indirec trend of trust: %f", trend.indirect))
                        trends.diff <- abs(trend.direct - trend.indirect)
                        # print(sprintf("diff trend of trust: %f", trends.diff))
                        trends.max <- max(abs(trend.direct), abs(trend.indirect))

                        if (trends.diff >= params$trust.rep.threshold - params$trust.rep.adj.range &&
                            trends.diff < trends.max + params$trust.rep.adj.range) {
                            trust.increment(recommendations[[i]]$id.sender)
                        } else if (trends.diff >= trends.max - params$trust.rep.adj.range &&
                                   trends.diff <= params$trend.threshold + params$trust.rep.adj.range) {
                            unknown.increment(recommendations[[i]]$id.sender)
                        } else {
                            distrust.increment(recommendations[[i]]$id.sender)
                        }
                    }
                }
            )
        },

        reputation.update = function() {
            "Update the reputations of the nodes"
            # TODO: Have cached context for each node, update here
            sapply(
                1:length(reputations),
                function(i) {
                    cur.rep <- weighted.trust(
                        compute.trust(trust[[i]], distrust[[i]], unknown[[i]]),
                        trust[[i]],
                        distrust[[i]],
                        unknown[[i]]
                    )
                    if (recommendations.cached[[i]]$valid) {
                        reputations.cached[[i]] <<- reputations[[i]]
                        reputations[[i]] <<- reputation.combination(
                            recommendations.cached[[i]]$context,
                            recommendations[[i]]$context,
                            reputations.cached[[i]],
                            cur.rep
                        )
                        # print(sprintf("rep %d: %f", i, reputations[[i]]))
                        if (abs(reputations[[i]]) <=
                            params$trust.rep.threshold + params$trust.rep.adj.range) {
                            reputations[[i]] <<- params$init.reputation
                        }
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
