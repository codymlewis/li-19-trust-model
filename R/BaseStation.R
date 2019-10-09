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
                next.hop = c(list(), rep(NULL, params$number.nodes)),
                hops = rep(Inf, params$number.nodes)
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
        },

        disconnect = function(device) {
            "Disconnect from a device"
            table$next.hop[[device$id]] <<- 0
            table$hops[[device$id]] <<- Inf
        },

        retabulate = function(device) {
            "Recalculate the routing tables of each of the neighbours"
            updated <<- TRUE
            for (neighbour in neighbours) {
                neighbour$tabulate.device(device, .self, table$hops[[device$id]] + 1)
            }
        },

        tabulate.device = function(device, prev.base.station, hops) {
            "Update the routing table on the given device"
            if (!updated) {
                table$next.hop[[device$id]] <<- prev.base.station
                table$hops[[device$id]] <<- hops
                updated <<- TRUE
                for (neighbour in neighbours) {
                    neighbour$tabulate.device(device, .self, hops + 1)
                }
            }
        },

        finish.update = function() {
            "Finish with updating the routing tables"
            if (updated) {
                updated <<- FALSE
                for (neighbour in neighbours) {
                    neighbour$finish.update()
                }
            }
        },

        find.device = function(dev.id) {
            "Route for the device with the given id"
            cur.device <- .self
            while (cur.device$table$hops[[dev.id]] > 0) {
                cur.device <- cur.device$table$next.hop[[dev.id]]
            }
            return (cur.device$table$next.hop[[dev.id]])
        }
    )
)
