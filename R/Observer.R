#' @include Device.R

Observer <- setRefClass(
    "Observer",
    contains = "Device",

    methods = list(
        transaction = function(devices) {
            "Perform a transaction with a service provider"
            normalized_c.target <- normalize(get_target_context())
            used_trust <- find_indirect_trust(normalized_c.target)
            prev_est_trust <- tail(estimated_trusts, 1)
            for (i in length(estimated_trusts):params$time_now) {
                if (i < params$time_now) {
                    estimated_trusts[[i]] <<- prev_est_trust
                } else {
                    estimated_trusts[[i]] <<- used_trust
                }
            }
        }
    )
)
