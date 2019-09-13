Tile <- setRefClass(
    "Tile",
    fields = list(
        objects = "list",
        signals = "list"
    ),
    methods = list(
        add.device = function(id, device) {
            objects[[id]] <<- device
        }

        rm.device = function(id) {
            objects[[id]] <<- NULL
        }

        add.signal = function(base.station) {
            signals[[length(signals) + 1]] = base.station
        }
    )
)
