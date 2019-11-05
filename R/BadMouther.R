#' @include Device.R

BadMouther <- setRefClass(
  "BadMouther",
  contains = "Device",

  methods = list(
    send.rec = function(devices) {
      emit.observation(
        Observation(
          contexts[[id]][get.context.index(params$time.now)],
          -1,
          id
        )
      )
    }
  )
)
