#' @include Device.R

BadMouther <- R6::R6Class(
    "BadMouther",
    inherit = Device,

    public = list(
        send_rec = function(devices) {
            self$emit_observation(
                Observation$new(
                    self$contexts[[self$id]][get_context_index(params$time_now)],
                    -1,
                    self$id
                )
            )
        }
    )
)
