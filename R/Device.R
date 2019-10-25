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
        sp.trust = "numeric",
        sp.distrust = "numeric",
        sp.unknown = "numeric",
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
        direct.observations = "list",
        indirect.observations = "list",
        weighted.avg.contexts = "list",
        latest.contexts = "list",
        dir.trust = "numeric",
        contexts = "list",
        stored.trusts = "list",
        trends.of.trust = "list",
        cached.contexts = "list"
    ),

    methods = list(
        initialize = function(id, sp, map, loc=round(runif(2, min=1, max=map.size))) {
            service.provider <<- sp
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
            last.rec.time <<- params$time.now
            time.last.moved <<- params$time.now
            estimated.trusts <<- c(params$trust.new.contact)
            direct.observations <<- list(
                Observation(
                    rep(0, length(params$context.weights)),
                    params$trust.new.contact,
                    id
                )
            )
            indirect.observations <<- lapply(
                1:params$number.nodes,
                function(i) {
                    Observation(
                        rep(0, length(params$context.weights)),
                        params$trust.new.contact,
                        i
                    )
                }
            )
            weighted.avg.contexts <<- lapply(
                1:params$number.nodes,
                function(i) { rep(0, length(params$context.weights)) }
            )
            latest.contexts <<- list()
            dir.trust <<- params$trust.new.contact
            contexts <<- lapply(
                1:params$number.nodes,
                function(i) {
                    `if`(
                        i == id,
                        normalize(
                            c(
                                params$time.now,
                                capability,
                                euc.dist(location, service.provider$location),
                                velocity
                            )
                        ),
                        c(params$time.now, 0, 0, 0)
                    )
                }
            )
            stored.trusts <<- lapply(
                1:params$number.nodes,
                function(i) { params$trust.new.contact }
            )
            reputations <<- sapply(
                1:params$number.nodes,
                function(i) {
                    `if`(
                        i == id,
                        params$rep.self,
                        params$init.reputation
                    )
                }
            )
            trends.of.trust <<- list()
            cached.contexts <<- lapply(
                1:params$number.nodes,
                function(i) { contexts[[i]] }
            )
        },

        init.recommendations = function() {
            "Create default observations for each of the nodes"
            return(
                sapply(
                    1:params$number.nodes,
                    function(i) {
                        Observation(
                            rep(0, length(params$context.weights)),
                            params$trust.new.contact,
                            i
                        )
                    }
                )
            )
        },

        add.contact = function(adds, devs) {
            for (i in adds) {
                if (length(contacts) > params$max.number.contacts) {
                    break
                }
                if (length(devs[[i]]$contacts) < params$max.number.contacts) {
                    contacts <<- c(contacts, i)
                    devs[[i]]$contacts <- c(devs[[i]]$contacts, id)
                }
            }
        },

        set.trusts = function() {
            "Set up the trusts for the service providers in the network"
            trust <<- rep(0, params$number.nodes)
            distrust <<- rep(0, params$number.nodes)
            unknown <<- rep(0, params$number.nodes)
            sp.trust <<- 0
            sp.distrust <<- 0
            sp.unknown <<- 0
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

        sp.trust.increment = function() {
            "Increment the trust count of the service provider"
            sp.trust <<- sp.trust + 1
        },

        sp.distrust.increment = function() {
            "Increment the distrust count of the service provider"
            sp.distrust <<- sp.distrust + 1
        },

        sp.unknown.increment = function() {
            "Increment the unknown count of the service provider"
            sp.unknown <<- sp.unknown + 1
        },

        recieve.observation = function(obs) {
            "Receive a recommendation from the sender"
            # performance.update(obs)
            combine.rep(obs)
            if ((length(contexts[[obs$id.sender]]) / length(params$context.weights)) >=
                params$compression.factor) {
                w.context <- find.weighted.context(
                    c(contexts[[obs$id.sender]], obs$context)
                )
                stored.trusts[[obs$id.sender]] <<- `if`(
                    obs$id.sender == id,
                    stored.trusts[[obs$id.sender]] <<- direct.trust(
                        c(stored.trusts[[obs$id.sender]], obs$trust),
                        c(contexts[[obs$id.sender]], obs$context),
                        w.context
                    ),
                    stored.trusts[[obs$id.sender]] <<- indirect.trust(
                        c(stored.trusts[[obs$id.sender]], obs$trust),
                        reputations[[obs$id.sender]],
                        c(contexts[[obs$id.sender]], obs$context),
                        find.weighted.context(contexts[[id]]),
                        w.context
                    )
                )
                contexts[[obs$id.sender]] <<- w.context
            } else {
                con.len <- length(contexts[[obs$id.sender]])
                cw.len <- length(params$context.weights)
                i <- `if`(
                    params$compression.factor < Inf,
                    length(stored.trusts[[obs$id.sender]]) + 1,
                    params$time.now
                )
                contexts[[obs$id.sender]][
                    (cw.len * (i - 1) + 1):(cw.len * i)
                ] <<- obs$context
                stored.trusts[[obs$id.sender]][i] <<- obs$trust
            }
        },

        should.consider.rec = function(rec) {
            "Check whether the recommendation should be considered"
            return (rec$trust > (params$delta.a - params$trust.rep.adj.range) ||
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
            context.target <- get.target.context()
            normalized.c.target <- normalize(context.target)
            rs.dir.trust <- find.direct.trust(context.target, normalized.c.target)
            # used.trust <- `if`(
            #     abs(rs.dir.trust$trust.evaled) <= rs.dir.trust$trust.est
            # )
            used.trust <- rs.dir.trust$trust.est
            if (used.trust > params$trust.rep.threshold - params$trust.rep.adj.range) {
                t.rs <- service.provider$provide.service()
                if (t.rs == TRUSTED) {
                    sp.trust.increment()
                } else if (t.rs == UNKNOWN) {
                    sp.unknown.increment()
                } else {
                    sp.distrust.increment()
                }
            }
            prev.est.trust <- tail(estimated.trusts, 1)
            for (i in length(estimated.trusts):params$time.now) {
                if (i < params$time.now) {
                    estimated.trusts[[i]] <<- prev.est.trust
                } else {
                    estimated.trusts[[i]] <<- used.trust
                }
            }
            recieve.observation(rs.dir.trust$obs)
            if (last.rec.time < params$time.now) {
                # send resulting observation to all contacts
                emit.observation(rs.dir.trust$obs, devices)
                last.rec.time <<- params$time.now
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
            trust.evaled <- weighted.trust(
                compute.trust(sp.trust, sp.distrust, sp.unknown),
                sp.trust,
                sp.distrust,
                sp.unknown
            )
            valid.trusts <- !is.na(stored.trusts[[id]])
            valid.contexts <- !is.na(contexts[[id]])
            context.weighted <- find.weighted.context(contexts[[id]][valid.contexts])
            dir.trust <<- direct.trust(
                c(stored.trusts[[id]][valid.trusts], trust.evaled),
                c(contexts[[id]][valid.contexts], context.weighted),
                context.weighted
            )
            return (
                list(
                    obs=Observation(
                        normalized.c.target,
                        trust.evaled,
                        id
                    ),
                    trust.est=estimate.trust(
                        normalized.c.target,
                        context.weighted,
                        dir.trust
                    ),
                    trust.evaled=trust.evaled
                )
            )
        },

        find.indirect.trust = function(context.target, normalized.c.target) {
            "Find the indirect trust of the service provider"
            context.weighted <- find.weighted.context(
                unlist(
                    lapply(
                        contacts,
                        function(i) { contexts[[i]][!is.na(contexts[[i]])] }
                    )
                )
            )
            # print(context.weighted)
            ind.trust <- sum(
                sapply(
                    contacts,
                    function(i) {
                        # TODO: find whether should be considered
                        # print("cached contexts")
                        # print(cached.contexts[[i]])
                        # print("stored trusts")
                        # print(stored.trusts[[i]])
                        indirect.trust(
                            stored.trusts[[i]][!is.na(stored.trusts[[i]])],
                            reputations[[i]],
                            contexts[[i]][!is.na(contexts[[i]])],
                            context.weighted,
                            cached.contexts[[i]]
                        )
                    }
                )
            )
            # print(ind.trust)
            return (
                estimate.trust(
                    normalized.c.target,
                    context.weighted,
                    ind.trust
                )
            )
        },

        performance.update = function(obs) {
            if (is.na(stored.trusts[[id]][params$time.now])) {
                context.now <- tail(contexts[[id]], length(params$context.weights))
                trust.now <- stored.trusts[[id]][[params$time.now]]
            } else {
                context.now <- sapply(
                    setdiff(contacts, c(id, obs$id.sender)),
                    function(i) {
                        tail(contexts[[i]], length(params$context.weights))
                    }
                )
                trust.now <- sapply(
                    setdiff(contacts, c(id, obs$id.sender)),
                    function(i) {
                        if (!is.na(stored.trusts[[i]][params$time.now])) {
                            stored.trusts[[i]][[params$time.now]]
                        }
                    }
                )
            }
        },

        combine.rep = function(obs) {
            "Find the new reputation for sender of recommendation"
            c.new <- find.weighted.context(
                c(cached.contexts[[obs$id.sender]], obs$context)
            )
            reputations[[obs$id.sender]] <<- reputation.combination(
                cached.contexts[[obs$id.sender]],
                obs$context,
                c.new,
                reputations[[obs$id.sender]],
                weighted.trust(
                    compute.trust(
                        trust[[obs$id.sender]],
                        distrust[[obs$id.sender]],
                        unknown[[obs$id.sender]]
                    ),
                    trust[[obs$id.sender]],
                    distrust[[obs$id.sender]],
                    unknown[[obs$id.sender]]
                )
            )
            if (abs(reputations[[obs$id.sender]]) <= params$trust.rep.adj.range) {
                reputations[[obs$id.sender]] <<- params$init.reputation
            }
            cached.contexts[[obs$id.sender]] <<- c.new
        },

        emit.observation = function(observation, devices) {
            "send observation to all contacts"
            for (contact in contacts) {
                connection.data <- communicate(contact)
                if (connection.data[[1]] < Inf) {
                    # routed communication
                    connection.data[[2]]$recieve.observation(observation)
                } else if (euc.dist(devices[[contact]]$location, location) <=
                           params$dev.signal.radius) {
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
