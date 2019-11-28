Params <- R6::R6Class(
    "Params",
    list(
        number_nodes = 0,
        number_good_nodes = 20,
        number_service_providers = 1,
        signal_radius = 100,
        dev_signal_radius = 14,
        max_number_contacts = 100,
        init_reputation = 0.01,
        rep_self = 1,
        trust_new_contact = 0,
        trust_rep_threshold = 0,
        trend_threshold = 0.01,
        trust_rep_adj_range = 0.001,
        sp_ground_trust = 1,
        max_capability = 100,
        map_width = 100,
        map_height = 100,
        max_velocity = 10,
        time_now = 1,
        context_weights = c(0.3, 0.2, 0.4, 0.1),
        eta = c(0.95, 0.7, 0.5, 0.5, 0.7),
        alpha = 0.3,
        beta = 0.3,
        gamma = 0.8,
        rho = 0.1,
        delta = 0.8,
        delta_a = -0.001,
        p_r = 1,
        theta_i = 0.8,
        impact_factor = 1,
        eta_i = 1,
        gap_factor = 2**-1,
        min_trans = 1,
        max_trans = 1,
        img_width = NULL,
        img_height = NULL,
        compression_factor = Inf,
        number_adversaries = 0,
        adversary_type = BadMouther,
        contacts_per_node = 15,
        rand_context = F,

        initialize = function() {
            self$number_nodes <- self$number_good_nodes + self$number_adversaries + 1
            self$img_width <- ceiling(5**(1 - self$map_width / 1000)) * self$map_width
            self$img_height <- ceiling(5**(1 - self$map_height / 1000)) * self$map_height
        },

        increment_time = function() {
            self$time_now <- self$time_now + 1
        }
    )
)
