Tile <- R6::R6Class(
    "Tile",
    list(
        objects = list(),
        signals = list(),
        terrain = NULL,
        base_station = list(),
        signal_edge = FALSE,

        initialize = function(terrain) {
            self$terrain <- terrain
        },

        add_device = function(device) {
            "Add a device here"
            self$objects[[device$id]] <- device
        },

        add_base_station = function(base_station) {
            "Add the base station here"
            self$base_station[[1]] <- base_station
        },

        get_base_station = function() {
            "Get the base station from here"
            return(self$base_station[[1]])
        },

        rm_device = function(id) {
            "Remove a device from here"
            self$objects[[id]] <- 0
        },

        has_devices = function() {
            "TRUE if there are devices on this tile, else FALSE"
            for (object in self$objects) {
                if (!is.numeric(object) && !is.null(object)) {
                    return(TRUE)
                }
            }
            return(FALSE)
        },

        add_signal = function(base_station, is_edge) {
            "Add a signal from a base station here"
            self$signals[[length(self$signals) + 1]] <- base_station
            if (is_edge) {
                signal_edge <<- TRUE
            }
        }
    )
)
