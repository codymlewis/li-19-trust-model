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
        initialize = function(context, trust, id.sender, valid=TRUE) {
            time <<- context[[1]]
            context <<- context
            trust <<- trust
            id.sender <<- id.sender
            valid <<- valid
        },

        update.time = function() {
            "Update the normalization of the time in this"
            context[[1]] <<- normalize.time(time)
        }
    )
)
