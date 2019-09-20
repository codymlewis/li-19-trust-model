LAND <- 0
WATER <- 1
AIR <- 2

# The parameters for the trust model
Params <- list(
    number.nodes = 200,
    signal.radius = 100,
    max.number.contacts = 100,
    init.reputation = 0.01,
    trust.new.contact = 0,
    trust.rep.threshold = 0,
    trend.threshold = 0.01,
    trust.rep.adj.range = 0.001,
    sp.ground.trust = 1,
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
    eta.i = 1
)