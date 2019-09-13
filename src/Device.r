Device <- setRefClass(
    "Device",
    fields = list(
        id="numeric",
        contacts="numeric",
        time="numeric",
        capability="double",
        velocity="double",
        trust="numeric",
        distrust="numeric",
        unknown="numeric",
        domain="character"
    ),

    methods = list(
        initialize = function() {
            trust <<- 0
            distrust <<- 0
            unknown <<- 0
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
