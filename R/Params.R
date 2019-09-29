LAND <- 0
WATER <- 1
AIR <- 2

# The parameters for the trust model
Params <- list(
    # number.nodes = 200,
    number.nodes = 10,
    number.service.providers = 1,
    signal.radius = 100,
    dev.signal.radius = 14,
    max.number.contacts = 100,
    init.reputation = 0.01,
    trust.new.contact = 0,
    trust.rep.threshold = 0,
    trend.threshold = 0.01,
    trust.rep.adj.range = 0.001,
    sp.ground.trust = 1,
    max.capability = 100,
    # map.width = 500,
    # map.height = 500,
    map.width = 100,
    map.height = 100,
    max.velocity = 10,
    time.now = 1,
    context.weights = c(0.3, 0.2, 0.4, 0.1),
    eta = c(0.95, 0.7, 0.5, 0.5, 0.7),
    alpha = 0.3,
    beta = 0.3,
    gamma = 0.8,
    rho = 0.1,
    delta = 0.8,
    delta.a = -0.001,
    p.r = 1,
    theta.i = 0.8,
    impact.factor = 1,
    eta.i = 1,
    gap.factor = 2**-1,
    transactions.per.time = 3
)
