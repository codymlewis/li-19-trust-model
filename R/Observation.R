Observation <- R6::R6Class(
    "Observation",
    list(
        context = NULL,
        trust = NULL,
        id_sender = NULL,

        initialize = function(context, trust, id_sender) {
            self$context <- context
            self$trust <- trust
            self$id_sender <- id_sender
        }
    )
)
