ServiceProvider <- setRefClass(
    "ServiceProvider",

    fields = list(
        id = "numeric",
        location = "numeric"
    ),

    methods = list(
        initialize = function(id=1) {
            id <<- id
            location <<- round(runif(2, min=1, max=c(Params$map.width, Params$map.height)))
        },

        provide.service = function() {
            return (TRUE)
        }
    )
)