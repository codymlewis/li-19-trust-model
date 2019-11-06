Tile <- setRefClass(
    "Tile",
    fields = list(
        objects = "list",
        signals = "list",
        terrain = "numeric",
        base_station = "list",
        signal_edge = "logical"
    ),

    methods = list(
        initialize = function(terrain) {
            objects <<- list()
            signals <<- list()
            terrain <<- terrain
            base_station <<- list()
            signal_edge <<- FALSE
        },

        add_device = function(device) {
            "Add a device here"
            objects[[device$id]] <<- device
        },

        add_base_station = function(base_station) {
            "Add the base station here"
            base_station[[1]] <<- base_station
        },

        get_base_station = function() {
            "Get the base station from here"
            return(base_station[[1]])
        },

        rm_device = function(id) {
            "Remove a device from here"
            objects[[id]] <<- 0
        },

        has_devices = function() {
            "TRUE if there are devices on this tile, else FALSE"
            for (object in objects) {
                if (!is.numeric(object) && !is.null(object)) {
                    return(TRUE)
                }
            }
            return(FALSE)
        },

        add_signal = function(base_station, is_edge) {
            "Add a signal from a base station here"
            signals[[length(signals) + 1]] <<- base_station
            if (is_edge) {
                signal_edge <<- TRUE
            }
        }
    )
)
