#' @include Device.R

BadMouther <- setRefClass(
    "BadMouther",
    contains = "Device",

    methods = list(
        send_rec = function(devices) {
            emit_observation(
                Observation(
                    contexts[[id]][get_context_index(params$time_now)],
                    -1,
                    id
                )
            )
        }
    )
)
