#' @include Params.R
#' @include Functions.R
#' @include Normalizers.R

Observation <- setRefClass(
    "Observation",

    fields = list(
        context = "numeric",
        trust = "numeric",
        id.sender = "numeric"
    ),

    methods = list(
        initialize = function(context, trust, id.sender) {
            context <<- normalize(context)
            trust <<- trust
            id.sender <<- id.sender
        },

        update.time = function() {
            context[[1]] <<- normalize.time(context[[1]])
        }
    )
)
