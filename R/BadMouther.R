#' @include Device.R

BadMouther <- R6::R6Class(
    "BadMouther",
    inherit = Device,

    public = list(
        send_rec = function(devices) {
            emit_observation(
                Observation$new(
                    contexts[[id]][get_context_index(params$time_now)],
                    -1,
                    id
                )
            )
        }
    )
)
