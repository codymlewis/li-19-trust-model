#' @include Params.R
#' @include Functions.R

Observation <- setRefClass(
    "Observation",

    fields = list(
        context = "numeric",
        time = "numeric",
        trust = "numeric",
        id.sender = "numeric",
        id.service.provider = "numeric"
    ),

    methods = list(
        initialize = function(context, trust, id.sender, id.service.provider) {
            time <<- context[[1]]
            context <<- sapply(
                1:length(context),
                function(i) {
                    normalizers[[i]](args[[i]])
                }
            )
            trust <<- trust
            id.sender <<- id.sender
            id.service.provider <<- id.service.provider
        },

        update.time = function() {
            context[[1]] <<- normalize.time(time)
        }
    )
)

normalize.time <- function(time)
{
    return (time / Params$time.now)
}

normalize.capability <- function(capability)
{
    return (1 - (capability / Params$max.capability))
}

normalize.location <- function(location)
{
    return (1 - (location / sqrt(Params$map.width**2 + Params$map.height**2)))
}

normalize.velocity <- function(velocity)
{
    return (1 - (velocity / Params$max.velocity))
}

normalizers <- c(
    normalize.time,
    normalize.capability,
    normalize.location,
    normalize.velocity
)
