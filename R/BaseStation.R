BaseStation <- setRefClass(
    "BaseStation",
    fields = list(
        location = "numeric",
        table = "list",
        neighbours = "list",
        updated = "logical"
    ),

    methods = list(
        initialize = function(x = 1, y = 1) {
            location <<- c(x, y)
            table <<- list(
                next_hop = c(list(), rep(NULL, params$number_nodes)),
                hops = rep(Inf, params$number_nodes)
            )
            neighbours <<- list()
            updated <<- FALSE
        },

        add_neighbour = function(base_station) {
            "Symmetrically add a new neighbouring base station to this"
            new_neighbour(base_station)
            base_station$new_neighbour(.self)
        },

        new_neighbour = function(neighbour) {
            "Add a new neighbour to this"
            neighbours[[length(neighbours) + 1]] <<- neighbour
        },

        connect = function(device) {
            "Connect to a device"
            table$next_hop[[device$id]] <<- device
            table$hops[[device$id]] <<- 0
        },

        disconnect = function(device) {
            "Disconnect from a device"
            table$next_hop[[device$id]] <<- 0
            table$hops[[device$id]] <<- Inf
        },

        retabulate = function(device) {
            "Recalculate the routing tables of each of the neighbours"
            updated <<- TRUE
            for (neighbour in neighbours) {
                neighbour$tabulate_device(device, .self, table$hops[[device$id]] + 1)
            }
        },

        tabulate_device = function(device, prev_base_station, hops) {
            "Update the routing table on the given device"
            if (!updated) {
                table$next_hop[[device$id]] <<- prev_base_station
                table$hops[[device$id]] <<- hops
                updated <<- TRUE
                for (neighbour in neighbours) {
                    neighbour$tabulate_device(device, .self, hops + 1)
                }
            }
        },

        finish_update = function() {
            "Finish with updating the routing tables"
            if (updated) {
                updated <<- FALSE
                for (neighbour in neighbours) {
                    neighbour$finish_update()
                }
            }
        },

        find_device = function(dev_id) {
            "Route for the device with the given id"
            cur_device <- .self
            while (cur_device$table$hops[[dev_id]] > 0) {
                cur_device <- cur_device$table$next_hop[[dev_id]]
            }
            return(cur_device$table$next_hop[[dev_id]])
        }
    )
)
