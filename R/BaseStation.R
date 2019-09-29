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
                next.hop = c(list(), rep(NULL, Params$number.nodes)),
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
                neighbour$tabulate.device(device, 1)
            }
            updated <<- FALSE
        },

        disconnect = function(device) {
            "Disconnect from a device"
            table$next.hop[[device$id]] <<- 0
            table$hops[[device$id]] <<- Inf
        },

        # TODO: Fix table update
        tabulate.device = function(device, hops) {
            "Update the routing table on the given device"
            if (!updated) {
                table$next.hop[[device$id]] <<- device
                table$hops[[device$id]] <<- hops
                updated <<- TRUE
                for (neighbour in neighbours) {
                    neighbour$tabulate.device(device, hops + 1)
                }
                updated <<- FALSE
            }
        },

        find.device = function(dev.id) {
            "Route for the device with the given id"
            return (table$next.hop[[dev.id]])
        }
    )
)
