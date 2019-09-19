Tile <- setRefClass(
    "Tile",
    fields = list(
        objects = "list",
        signals = "list",
        terrain = "numeric"
    ),
    methods = list(
        initialize = function(terrain) {
            terrain <<- terrain
        },

        add.device = function(id, device) {
            objects[[id]] <<- device
        },

        rm.device = function(id) {
            objects[[id]] <<- NULL
        },

        add.signal = function(base.station) {
            signals[[length(signals) + 1]] <<- base.station
        }
    )
)

#' A Field Class
#'
#' This class is a field containing the base stations and their signals
#' @keywords Map Base Station
#' @export Field
#' @exportClass Field
#' @examples
#' Field(c(BaseStation(1, 1), BaseStation(2, 2)))

Field <- setRefClass(
    "Field",
    fields = list(
        tiles = "list"
    ),

    methods = list(
        initialize = function(base.stations, width=500, height=500) {
            tiles <<- list()
            cat("Creating field...\n")
            for (i in 1:width) {
                tiles[[i]] <<- list()
                for (j in 1:height) {
                    tiles[[i]][[j]] <<- Tile(`if`(round(runif(1)), 1, 0))
                    for (base.station in base.stations) {
                        if (euc.dist(base.station$location, c(i, j)) < 100) {
                            tiles[[i]][[j]]$add.signal(base.station)
                        }
                    }
                }
                cat.progress(
                        i, width, prefix=sprintf("Column %d of %d", i, width)
                )
            }
        },

        size = function() {
            "Get the size of the field"
            return (length(tiles))
        },

        shape = function() {
            return (c(length(tiles), length(tiles[[1]])))
        },

        get.tile = function(location) {
            "Get the tile at the location if there is one, otherwise NA"
            if (all(location <= shape()) && all(location >= c(0, 0))) {
                return (tile[[location[[1]]]][[location[[2]]]])
            }
            return (NA)
        }
    )
)

place.basestations = function(width, height)
{
    gap <- sqrt(2 * 100**2) / 2
    base.stations <- rep()
}
