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
        table = "list"
    ),

    methods = list(
        initialize = function(x, y) {
            location <<- c(x, y)
            table <<- list()
        },

        connect = function(device) {
            table[[device$id]] <<- device
        },

        calculate.table = function() {
            # Djiksta's modified
        }
    )
)
