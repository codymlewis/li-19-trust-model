#' @include Params.R
#' @include Functions.R

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
            terrain <<- terrain
            base.station <<- list()
        },

        add.device = function(id, device) {
            objects[[id]] <<- device
        },

        add.base.station = function(base.station) {
            base.station[[1]] <<- base.station
        },

        get.base.station = function() {
            return (base.station[[1]])
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
#' Field(read.csv("map.csv"))

Field <- setRefClass(
    "Field",

    fields = list(
        tiles = "list"
    ),

    methods = list(
        initialize = function(data=NULL) {
            base.stations <- place.base.stations(Params$map.width, Params$map.height)
            tiles <<- list()
            cat("Creating field...\n")
            for (i in 1:Params$map.width) {
                tiles[[i]] <<- list()
                for (j in 1:Params$map.height) {
                    tiles[[i]][[j]] <<- Tile(
                        `if`(
                            !all(is.null(data)),
                            data[[i]][[j]],
                            `if`(round(runif(1)), 1, 0)
                        )
                    )
                    for (base.station in base.stations) {
                        if (euc.dist(base.station$location, c(i, j)) < 100) {
                            tiles[[i]][[j]]$add.signal(base.station)
                        }
                        if (all(base.station$location == c(i, j))) {
                            tiles[[i]][[j]]$add.base.station(base.station)
                        }
                    }
                }
                cat.progress(
                    i, Params$map.width, prefix=sprintf("Column %d of %d", i, Params$map.width)
                )
            }
            grid.connect(.self, base.stations)
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
            print(shape())
            if (all(location <= shape()) && all(location > c(0, 0))) {
                return (list(tiles[[location[[1]]]][[location[[2]]]]))
            }
            print("here")
            return (list())
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


grid.connect <- function(field, base.stations)
{
    gap <- compute.gap(Params$signal.radius)
    for (base.station in base.stations) {
        cur.loc <- base.station$location
        other.tile <- field$get.tile(cur.loc - c(gap, 0))
        if (length(other.tile)) {
            other.station <- other.tile[[1]]$get.base.station()
            check.and.add.neighbour(base.station, other.station)
        }
        other.tile <- field$get.tile(cur.loc - c(0, gap))
        if (length(other.tile)) {
            other.station <- other.tile[[1]]$get.base.station()
            check.and.add.neighbour(base.station, other.station)
        }
    }
}


check.and.add.neighbour <- function(base.station, other.station)
{
    base.station$add.neighbour(other.station)
}
