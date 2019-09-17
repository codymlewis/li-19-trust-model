# source("R/Functions.R")

Tile <- setRefClass(
    "Tile",
    fields = list(
        objects = "list",
        signals = "list",
        terrain = "character"
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

Map <- setRefClass(
    "Map",
    fields = list(
        tiles = "list"
    ),

    methods = list(
        initialize = function(map, base.stations) {
            tiles <<- list()
            cat("Creating map...\n")
            for (i in 1:ncol(map)) {
                tiles[[i]] <<- list()
                for (j in 1:nrow(map)) {
                    tiles[[i]][[j]] <<- Tile(as.character(map[[i]][[j]]))
                    for (base.station in base.stations) {
                        if (euc.dist(base.station$location, c(i, j)) < 100) {
                            tiles[[i]][[j]]$add.signal(base.station)
                        }
                    }
                }
                cat.progress(i, ncol(map), prefix=sprintf("Column %d of %d", i, ncol(map)))
            }
        },

        size = function() {
            "Get the size of the map"
            return (length(tiles))
        },

        get.tile = function(location) {
            "Get the tile at the location if there is one, otherwise NA"
            if (location[[1]] <= length(tiles) &&
                location[[2]] <= length(tiles[[location[[1]]]])) {
                return (tile[[location[[1]]]][[location[[2]]]])
            }
            return (NA)
        }
    )
)
