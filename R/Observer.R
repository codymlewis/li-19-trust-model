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
            if (used_trust > params$trust_rep_threshold - params$trust_rep_adj_range) {
                t_rs <- self$service_provider$provide_service()
                if (t_rs == TRUSTED) {
                    self$sp_trust_increment()
                } else if (t_rs == UNKNOWN) {
                    self$sp_unknown_increment()
                } else {
                    self$sp_distrust_increment()
                }
            }
            for (i in length(self$estimated_trusts):params$time_now) {
                if (i < params$time_now) {
                    self$estimated_trusts[[i]] <- prev_est_trust
                } else {
                    self$estimated_trusts[[i]] <- used_trust
                }
            }
            self$recieve_observation(
                Observation$new(
                    normalized_c_target,
                    weighted_trust(
                        compute_trust(self$sp_trust, self$sp_distrust, self$sp_unknown),
                        self$sp_trust,
                        self$sp_distrust,
                        self$sp_unknown
                    ),
                    self$id
                )
            )
        },

        send_rec = function(devices) {

        }
    )
)
