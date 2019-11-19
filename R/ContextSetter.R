#' @include Device.R

ContextSetter <- R6::R6Class(
    "ContextSetter",
    inherit = Device,

    public = list(
        send_rec = function(devices) {
            self$emit_observation(
                Observation$new(
                    c(params$time_now, 0.5, 0.5, 0.5),
                    -1,
                    self$id
                )
            )
        }
    )
)
