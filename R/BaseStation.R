#' @include Params.R

#' A Base Station Class
#'
#' This class defines the base stations within the trust model, these are
#' essentially gateways.
#' @keywords Base Station Gateway
#' @export BaseStation
#' @exportClass BaseStation
#' @examples
#' BaseStation(1, 1)

BaseStation <- setRefClass(
    "BaseStation",
    fields = list(
        location = "numeric",
        table = "list",
        neighbours = "list",
        updated = "logical"
    ),

    methods = list(
        initialize = function(x=1, y=1) {
            location <<- c(x, y)
            table <<- list(
                next.hop = c(list(), rep(NA, Params$number.nodes)),
                hops = rep(Inf, Params$number.nodes)
            )
            neighbours <<- list()
            updated <<- FALSE
        },

        add.neighbour = function(base.station) {
            "Symmetrically add a new neighbouring base station to this"
            new.neighbour(base.station)
            base.station$new.neighbour(.self)
        },

        new.neighbour = function(neighbour) {
            "Add a new neighbour to this"
            neighbours[[length(neighbours) + 1]] <<- neighbour
        },

        connect = function(device) {
            "Connect to a device"
            table$next.hop[[device$id]] <<- device
            table$hops[[device$id]] <<- 0
            updated <<- TRUE
            for (neighbour in neighbours) {
                neighbour$tabulate.device(device, .self, 1)
            }
        },

        disconnect = function(device) {
            "Disconnect from a device"
            table$next.hop[[device$id]] <<- NA
            table$hops[[device$id]] <<- Inf
        },

        tabulate.device = function(device, prev.base.station, hops) {
            "Update the routing table on the given device"
            if (table$hops[[device$id]] == 0 || (updated && table$hops[[device$id]] < hops)) {
                true.hops <- table$hops[[device$id]]
            } else {
                true.hops <- hops
                table$next.hop[[device$id]] <<- prev.base.station
                table$hops[[device$id]] <<- true.hops
            }

            updated <<- TRUE
            for (neighbour in neighbours) {
                neighbour$tabulate.device(device, .self, true.hops + 1)
            }
        },

        find.device = function(dev.id) {
            "Route for the device with the given id"
            cur.device <- .self
            for (i in 0:table$hops[[dev.id]]) {
                cur.device <- cur.device$table$next.hop[[dev.id]]
            }
            return (cur.device)
        }
    )
)
