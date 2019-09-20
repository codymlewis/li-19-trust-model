#' @include Params.R
#' @include Functions.R

Tile <- setRefClass(
    "Tile",
    fields = list(
        objects = "list",
        signals = "list",
        terrain = "numeric",
        base.station = "BaseStation"
    ),
    methods = list(
        initialize = function(terrain) {
            terrain <<- terrain
            base.station <<- NA
        },

        add.device = function(id, device) {
            objects[[id]] <<- device
        },

        add.base.station = function(base.station) {
            base.station <<- base.station
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
#' Field()
#' Field(300, 300)

Field <- setRefClass(
    "Field",
    fields = list(
        tiles = "list"
    ),

    methods = list(
        initialize = function(width=500, height=500) {
            base.stations <- place.base.stations(width, height)
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
            grid.connect(tiles, base.stations)
        },

        size = function() {
            "Get the size of the field"
            return (length(tiles))
        },

        shape = function() {
            "Get the shape of the field"
            return (c(length(tiles), length(tiles[[1]])))
        },

        get.tile = function(location) {
            "Get the tile at the location if there is one, otherwise NA"
            if (all(location <= shape()) && all(location > c(0, 0))) {
                return (tile[[location[[1]]]][[location[[2]]]])
            }
            return (NA)
        }
    )
)


# Place the base stations on a rectangle such that the signals cover the
# entirety of the rectangle
place.base.stations <- function(width, height)
{
    gap <- compute.gap(Params$signal.radius)
    base.stations <- list()

    for (i in seq(1, width, gap)) {
        for (j in seq(1, height, gap)) {
            base.stations[[length(base.stations) + 1]] <- BaseStation(i, j)
        }
    }

    return (base.stations)
}


compute.gap <- function(radius)
{
    return (round(sqrt(2 * radius**2) / 2))
}


grid.connect <- function(tiles, base.stations)
{
    gap <- compute.gap(Params$signal.radius)
    for (base.station in base.stations) {
        cur.loc <- base.station$location
        other.station <- tile$get.tile(cur.loc - c(gap, 0))
        check.and.add.neighbour(base.station, other.station)
        other.station <- tiles$get.tile(cur.loc - c(0, gap))
        check.and.add.neighbour(base.station, other.station)
    }
}


check.and.add.neighbour <- function(base.station, other.station)
{
    if (!is.na(other.station)) {
        base.station$add.neighbour(other.station)
    }
}
