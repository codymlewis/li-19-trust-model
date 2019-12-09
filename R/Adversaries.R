BadMouther <- R6::R6Class(
    "BadMouther",
    inherit = Device,
    public = list(
        send_rec = function(devices) {
            rs_dir_trust <- self$find_direct_trust(
                self$contexts[[self$id]][get_context_index(params$time_now)]
            )
            self$stored_trusts[[self$id]][[params$time_now]] <- rs_dir_trust$trust_comb
            self$emit_observation(
                Observation$new(
                    self$contexts[[self$id]][get_context_index(params$time_now)],
                    -1,
                    self$id
                ),
                devices
            )
        }
    )
)


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
