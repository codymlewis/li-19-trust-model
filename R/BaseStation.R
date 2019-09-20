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
        initialize = function(x, y) {
            location <<- c(x, y)
            table <<- list(
                next.hop = c(list(), rep(NA, Params$number.nodes)),
                hops = rep(Inf, Params$number.nodes)
            )
            neighbours <<- list()
            updated <<- FALSE
        },

        add.neighbour = function(base.station) {
            "Add a new neighbouring base station to this"
            # TODO: Make this symmetric
            neighbours[[length(neighbours) + 1]] <<- base.station
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
        }
    )
)
