LAND <- 0
WATER <- 1
AIR <- 2

Params <- setRefClass(
    "Params",

    fields = list(
        number.nodes = "numeric",
        number.service.providers = "numeric",
        signal.radius = "numeric",
        dev.signal.radius = "numeric",
        max.number.contacts = "numeric",
        init.reputation = "numeric",
        rep.self = "numeric",
        trust.new.contact = "numeric",
        trust.rep.threshold = "numeric",
        trend.threshold = "numeric",
        trust.rep.adj.range = "numeric",
        sp.ground.trust = "numeric",
        max.capability = "numeric",
        map.width = "numeric",
        map.height = "numeric",
        max.velocity = "numeric",
        time.now = "numeric",
        context.weights = "numeric",
        eta = "numeric",
        alpha = "numeric",
        beta = "numeric",
        gamma = "numeric",
        rho = "numeric",
        delta = "numeric",
        delta.a = "numeric",
        p.r = "numeric",
        theta.i = "numeric",
        impact.factor = "numeric",
        eta.i = "numeric",
        gap.factor = "numeric",
        transactions.per.time = "numeric",
        ignore.bad.rec.time = "numeric",
        img.width = "numeric",
        img.height = "numeric",
        compression.factor = "numeric",
        contacts.per.node = "numeric"
    ),

    methods = list(
        initialize = function() {
            # number.nodes <<- 200
            number.nodes <<- 20
            number.service.providers <<- 1
            signal.radius <<- 100
            # signal.radius <<- 50
            dev.signal.radius <<- 14
            max.number.contacts <<- 100
            init.reputation <<- 0.01
            rep.self <<- 1
            # init.reputation <<- 1
            trust.new.contact <<- 0
            trust.rep.threshold <<- 0
            trend.threshold <<- 0.01
            trust.rep.adj.range <<- 0.001
            sp.ground.trust <<- 1
            max.capability <<- 100
            # map.width <<- 500
            # map.height <<- 500
            map.width <<- 100
            map.height <<- 100
            max.velocity <<- 10
            time.now <<- 1
            context.weights <<- c(0.3, 0.2, 0.4, 0.1)
            eta <<- c(0.95, 0.7, 0.5, 0.5, 0.7)
            alpha <<- 0.3
            beta <<- 0.3
            gamma <<- 0.8
            rho <<- 0.1
            delta <<- 0.8
            delta.a <<- -0.001
            p.r <<- 1
            theta.i <<- 0.8
            impact.factor <<- 1
            eta.i <<- 1
            gap.factor <<- 2**-1
            transactions.per.time <<- 3
            ignore.bad.rec.time <<- 200
            # img.width <<- 1000
            # img.height <<- 1000
            img.width <<- 500
            img.height <<- 500
            compression.factor <<- Inf
            # compression.factor <<- 5
            # contacts.per.node <- sqrt(min(params$max.number.contacts, params$number.nodes - 1))
            contacts.per.node <<- 10
        },

        increment.time = function() {
            time.now <<- time.now + 1
        }
    )
)

params <- Params()
