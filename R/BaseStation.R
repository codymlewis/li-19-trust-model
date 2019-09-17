BaseStation <- setRefClass(
    "BaseStation",
    fields = list(
        location = "numeric",
        table = "list"
    ),

    methods = list(
        initialize = function(x, y) {
            location <<- c(x, y)
            table <<- list()
        },

        connect = function(device) {
            table[[device$id]] <<- device
        },

        calculate.table = function() {
            # Djiksta's modified
        }
    )
)
