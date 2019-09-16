source("Map.r")

Device <- setRefClass(
    "Device",
    fields = list(
        id="numeric",
        contacts="numeric",
        location="Tile",
        time="numeric",
        capability="double",
        velocity="double",
        trust="numeric",
        distrust="numeric",
        unknown="numeric",
        domain="character"
    ),

    methods = list(
        initialize = function(no_contacts=200) {
            trust <<- 0
            distrust <<- 0
            unknown <<- 0
            contacts <<- sample(1:no_contacts, round(runif(1, min=1, max=100)))
        },

        trust.increment = function() {
            trust <<- trust + 1
        },

        distrust.increment = function() {
            distrust <<- distrust + 1
        },

        unknown.increment = function() {
           unknown <<- unknown + 1
        }
    )
)
