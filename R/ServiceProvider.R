TRUSTED <- 0
UNKNOWN <- 1
DISTRUST <- 2

ServiceProvider <- setRefClass(
  "ServiceProvider",
  fields = list(
    id = "numeric",
    location = "numeric",
    transaction.results = "numeric"
  ),

  methods = list(
    initialize = function(id = 1, p.trust = 1, p.unknown = 0, p.distrust = 0) {
      id <<- id
      location <<- round(runif(2, min = 1, max = c(params$map.width, params$map.height)))
      p.vals <- c(p.trust, p.unknown, p.distrust)
      sample.factor <- 10**(1 - floor(log(min(p.vals[p.vals != 0]), base = 10)))
      transaction.results <<- c(
        rep(TRUSTED, p.trust * sample.factor),
        rep(UNKNOWN, p.unknown * sample.factor),
        rep(DISTRUST, p.distrust * sample.factor)
      )
    },

    provide.service = function() {
      return(sample(transaction.results, 1))
    }
  )
)
