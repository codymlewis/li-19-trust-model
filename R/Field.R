#' @include Params.R
#' @include Functions.R
#' @include Tile.R

Field <- setRefClass(
    "Field",

    fields = list(
        tiles = "list"
    ),

    methods = list(
        initialize = function(data=NULL, verbose=F) {
            base.stations <- place.base.stations(params$map.width, params$map.height)
            tiles <<- list()
            if (verbose) {
                cat("Creating field...\n")
            }
            for (i in 1:params$map.width) {
                tiles[[i]] <<- list()
                for (j in 1:params$map.height) {
                    tiles[[i]][[j]] <<- Tile(
                        `if`(
                            !all(is.null(data)),
                            data[[i]][[j]],
                            `if`(round(runif(1)), 1, 0)
                        )
                    )
                    for (base.station in base.stations) {
                        if (euc.dist(base.station$location, c(i, j)) <= params$signal.radius) {
                            is.edge <- (euc.dist(base.station$location, c(i + 1, j)) > params$signal.radius) ||
                                (euc.dist(base.station$location, c(i, j + 1)) > params$signal.radius) ||
                                (euc.dist(base.station$location, c(i - 1, j)) > params$signal.radius) ||
                                (euc.dist(base.station$location, c(i, j - 1)) > params$signal.radius)
                            tiles[[i]][[j]]$add.signal(
                                base.station,
                                is.edge
                            )
                        }
                        if (all(base.station$location == c(i, j))) {
                            tiles[[i]][[j]]$add.base.station(base.station)
                        }
                    }
                }
                if (verbose) {
                    cat.progress(
                        i,
                        params$map.width,
                        prefix=sprintf("Column %d of %d", i, params$map.width)
                    )
                }
            }
            grid.connect(.self, base.stations)
        },

        size = function() {
            "Get the size of the field"
            return (length(tiles))
        },

        shape = function() {
            "Get the shape of the field"
            return (c(length(tiles[[1]]), length(tiles)))
        },

        get.tile = function(location) {
            "Get the tile at the location if there is one, otherwise NA"
            if (all(location <= shape()) && all(location > c(0, 0))) {
                return (list(tiles[[location[[1]]]][[location[[2]]]]))
            }
            return (list())
        }
    )
)


# Place the base stations on a rectangle such that the signals cover the
# entirety of the rectangle
place.base.stations <- function(width, height)
{
    gap <- compute.gap(params$signal.radius)
    base.stations <- list()

    for (i in seq(min(width / 2, gap), width, gap)) {
        for (j in seq(min(height / 2, gap), height, gap)) {
            base.stations[[length(base.stations) + 1]] <- BaseStation(i, j)
        }
    }

    return (base.stations)
}


grid.connect <- function(field, base.stations)
{
    gap <- compute.gap(params$signal.radius)
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
