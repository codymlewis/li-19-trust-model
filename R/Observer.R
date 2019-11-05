#' @include Device.R

Observer <- setRefClass(
  "Observer",
  contains = "Device",

  methods = list(
    transaction = function(devices) {
      "Perform a transaction with a service provider"
      normalized.c.target <- normalize(get.target.context())
      used.trust <- find.indirect.trust(normalized.c.target)
      prev.est.trust <- tail(estimated.trusts, 1)
      for (i in length(estimated.trusts):params$time.now) {
        if (i < params$time.now) {
          estimated.trusts[[i]] <<- prev.est.trust
        } else {
          estimated.trusts[[i]] <<- used.trust
        }
      }
    }
  )
)
