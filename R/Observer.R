Observer <- R6::R6Class(
    "Observer",
    inherit = Device,

    public = list(
        use_trust = function(normalized_c_target) {
            return(self$find_indirect_trust(normalized_c_target))
        }
    )
)
