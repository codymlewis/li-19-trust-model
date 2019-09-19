Device <- setRefClass(
    "Device",
    fields = list(
        id="numeric",
        contacts="numeric",
        location="numeric",
        current.goal="numeric",
        time="numeric",
        capability="numeric",
        velocity="numeric",
        trust="numeric",
        distrust="numeric",
        unknown="numeric",
        domain="character"
    ),

    methods = list(
        initialize = function(id, map, no.contacts=200) {
            id <<- id
            trust <<- 0
            distrust <<- 0
            unknown <<- 0
            contacts <<- sample(1:no.contacts, round(runif(1, min=1, max=100)))
            location <<- round(runif(2, min=0, max=(map$size() - 1)))
            map$get.tile(location)$add.device(id, .self)
            velocity <<- runif(1, min=0, max=10)
            current.goal <<- round(runif(2, min=0, max=(map.size - 1)))
            if (round(runif(1)) == 1) {
                domain <<- "a"
            } else {
                domain <<- map[[location[[1]]]][[location[[2]]]]$terrain
            }
        },

        trust.increment = function() {
            trust <<- trust + 1
        },

        distrust.increment = function() {
            distrust <<- distrust + 1
        },

        unknown.increment = function() {
           unknown <<- unknown + 1
        },

        move = function(time.change, map) {
            "Move towards the current goal"
            movement.amount <- round(velocity * time.change)
            movement <- `if`(movement.amount > 0, 1:movement.amount, NULL)
            for (m in movement) {
                best.weight <- Inf
                best.loc <- NA
                best.tile <- NA
                for (i in location[[1]] - 1:location[[1]] + 1) {
                    for (j in location[[2]] - 1:location[[2]] + 1) {
                        tile <- map$get.tile(c(i, j))
                        loc <- c(i, j)
                        if (!(all(loc %in% location)) && !is.na(tile)) {
                            cost <- `if`(
                                domain == "a",
                                1,
                                `if`(
                                    domain == tile$terrain,
                                    1,
                                    2
                                )
                            )
                            weight <- cost + euc.dist(loc, current.goal)
                            if (weight < best.weight) {
                                best.weight <- weight
                                best.loc <- loc
                                best.tile <- tile
                            }
                        }
                    }
                }
                map$get.tile(location)$rm.device(id)
                location <<- best.loc
                best.tile$add.device(id, .self)
            }
            velocity <<- max(0, velocity + rnorm(1))
            if (all(location %in% current.goal)) {
                current.goal <<- round(runif(2, min=0, max=(map$size() - 1)))
            }
            # TODO: update routing tables
        },

        communicate = function(map, device.loc) {
            "Communicate with a random contact"
            id.other <- sample(contacts, 1)
            if (euc.dist(location, device.loc[id.other,]) < 100) {
                # neighbour communication
            } else {
                # routed communication
            }
        }
    )
)
