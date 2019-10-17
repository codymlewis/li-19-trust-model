#' @include Params.R
#' @include Functions.R
#' @include Normalizers.R

Observation <- setRefClass(
    "Observation",

    fields = list(
        time = "numeric",
        context = "numeric",
        trust = "numeric",
        id.sender = "numeric",
        valid = "logical"
    ),

    methods = list(
        initialize = function(context, trust, id.sender) {
            context <<- context
            trust <<- trust
            id.sender <<- id.sender
        },

        get.context = function() {
            return (context)
        }
    )
)
