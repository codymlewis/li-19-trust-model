Observation <- setRefClass(
    "Observation",
    fields = list(
        context = "numeric",
        trust = "numeric",
        id_sender = "numeric"
    ),

    methods = list(
        initialize = function(context, trust, id_sender) {
            context <<- context
            trust <<- trust
            id_sender <<- id_sender
        }
    )
)
