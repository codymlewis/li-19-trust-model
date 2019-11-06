LAND <- 0
WATER <- 1
AIR <- 2

Params <- setRefClass(
    "Params",
    fields = list(
        number_nodes = "numeric",
        number_good_nodes = "numeric",
        number_service_providers = "numeric",
        signal_radius = "numeric",
        dev_signal_radius = "numeric",
        max_number_contacts = "numeric",
        init_reputation = "numeric",
        rep_self = "numeric",
        trust_new_contact = "numeric",
        trust_rep_threshold = "numeric",
        trend_threshold = "numeric",
        trust_rep_adj_range = "numeric",
        sp_ground_trust = "numeric",
        max_capability = "numeric",
        map_width = "numeric",
        map_height = "numeric",
        max_velocity = "numeric",
        time_now = "numeric",
        context_weights = "numeric",
        eta = "numeric",
        alpha = "numeric",
        beta = "numeric",
        gamma = "numeric",
        rho = "numeric",
        delta = "numeric",
        delta_a = "numeric",
        p_r = "numeric",
        theta_i = "numeric",
        impact_factor = "numeric",
        eta_i = "numeric",
        gap_factor = "numeric",
        transactions_per_time = "numeric",
        ignore_bad_rec_time = "numeric",
        img_width = "numeric",
        img_height = "numeric",
        compression_factor = "numeric",
        contacts_per_node = "numeric"
    ),

    methods = list(
        initialize = function() {
            # number_nodes <<- 200
            number_nodes <<- 21
            number_good_nodes <<- 20
            number_service_providers <<- 1
            signal_radius <<- 100
            # signal_radius <<- 50
            dev_signal_radius <<- 14
            max_number_contacts <<- 100
            init_reputation <<- 0.01
            rep_self <<- 1
            # init_reputation <<- 1
            trust_new_contact <<- 0
            trust_rep_threshold <<- 0
            trend_threshold <<- 0.01
            trust_rep_adj_range <<- 0.001
            sp_ground_trust <<- 1
            max_capability <<- 100
            # map_width <<- 500
            # map_height <<- 500
            map_width <<- 100
            map_height <<- 100
            max_velocity <<- 10
            time_now <<- 1
            context_weights <<- c(0.3, 0.2, 0.4, 0.1)
            eta <<- c(0.95, 0.7, 0.5, 0.5, 0.7)
            alpha <<- 0.3
            beta <<- 0.3
            gamma <<- 0.8
            rho <<- 0.1
            delta <<- 0.8
            delta_a <<- -0.001
            p_r <<- 1
            theta_i <<- 0.8
            impact_factor <<- 1
            eta_i <<- 1
            gap_factor <<- 2**-1
            transactions_per_time <<- 3
            ignore_bad_rec_time <<- 200
            # img_width <<- 1000
            # img_height <<- 1000
            img_width <<- 500
            img_height <<- 500
            compression_factor <<- Inf
            # compression_factor <<- 5
            # contacts_per_node <- sqrt(min(params$max_number_contacts, params$number_nodes - 1))
            contacts_per_node <<- 10
        },

        increment_time = function() {
            time_now <<- time_now + 1
        }
    )
)

params <- Params()
