Tile <- setRefClass(
    "Tile",

    fields = list(
        objects = "list",
        signals = "list",
        terrain = "numeric",
        base.station = "list"
    ),

    methods = list(
        initialize = function(terrain) {
            objects <<- list()
            signals <<- list()
            terrain <<- terrain
            base.station <<- list()
        },

        add.device = function(device) {
            "Add a device here"
            objects[[device$id]] <<- device
        },

        add.base.station = function(base.station) {
            "Add the base station here"
            base.station[[1]] <<- base.station
        },

        get.base.station = function() {
            "Get the base station from here"
            return (base.station[[1]])
        },

        rm.device = function(id) {
            "Remove a device from here"
            objects[[id]] <<- NULL
        },

        add.signal = function(base.station) {
            "Add a signal from a base station here"
            signals[[length(signals) + 1]] <<- base.station
        }
    )
)
