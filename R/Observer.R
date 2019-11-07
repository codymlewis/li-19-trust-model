#' @include Device.R

Observer <- R6::R6Class(
    "Observer",
    inherit = Device,

    public = list(
        transaction = function(devices) {
            "Perform a transaction with a service provider"
            normalized_c_target <- normalize(self$get_target_context())
            rs_dir_trust <- self$find_direct_trust(normalized_c_target)
            used_trust <- self$find_indirect_trust(normalized_c_target)
            prev_est_trust <- tail(self$estimated_trusts, 1)
            for (i in length(self$estimated_trusts):params$time_now) {
                if (i < params$time_now) {
                    self$estimated_trusts[[i]] <- prev_est_trust
                } else {
                    self$estimated_trusts[[i]] <- used_trust
                }
            }
            self$recieve_observation(rs_dir_trust$obs)
        },

        send_rec = function(devices) {

        }
    )
)
