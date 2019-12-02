Device <- R6::R6Class(
    "Device",
    list(
        id = NULL,
        contacts = NULL,
        location = NULL,
        current_goal = NULL,
        capability = NULL,
        velocity = NULL,
        trust = NULL,
        distrust = NULL,
        unknown = NULL,
        sp_trust = NULL,
        sp_distrust = NULL,
        sp_unknown = NULL,
        domain = NULL,
        reputations = NULL,
        service_provider = NULL,
        time_last_moved = NULL,
        estimated_trusts = NULL,
        map = list(),
        contexts = list(),
        stored_trusts = list(),
        cached_contexts = list(),

        initialize = function(id, sp, map, loc = round(runif(2, min = 1, max = map_size))) {
            self$service_provider <- sp
            if (!is.null(map)) {
                map_size <- map$shape()
                self$map <- list(map)
                self$location <- loc
                if (round(runif(1)) == 1) {
                    self$domain <- AIR
                } else {
                    self$domain <- map$get_tile(self$location)[[1]]$terrain
                }
                self$new_goal()
            } else {
                map_size <- c(params$map_width, params$map_height)
                self$location <- loc
                self$map <- list()
                self$domain <- sample(c(AIR, LAND, WATER), 1)
            }
            self$id <- id
            self$set_trusts()
            if (!is.null(map)) {
                map$get_tile(self$location)[[1]]$add_device(self)
                for (signal in map$get_tile(self$location)[[1]]$signals) {
                    signal$connect(self)
                }
            }
            self$velocity <- runif(1, min = 0, max = params$max_velocity)
            self$capability <- runif(1, min = 1, max = params$max_capability)
            self$reputations <- rep(params$init_reputation, params$number_nodes)
            self$time_last_moved <- params$time_now - 1
            self$estimated_trusts <- c(params$trust_new_contact)
            self$contexts <- lapply(
                1:params$number_nodes,
                function(i) {
                    `if`(
                        i == id,
                        normalize(
                            c(
                                params$time_now,
                                self$capability,
                                euc_dist(self$location, self$service_provider$location),
                                self$velocity
                            )
                        ),
                        c(params$time_now, 0, 0, 0)
                    )
                }
            )
            self$stored_trusts <- lapply(
                1:params$number_nodes,
                function(i) {
                    params$trust_new_contact
                }
            )
            self$reputations <- sapply(
                1:params$number_nodes,
                function(i) {
                    `if`(
                        i == id,
                        params$rep_self,
                        params$init_reputation
                    )
                }
            )
            self$cached_contexts <- lapply(
                1:params$number_nodes,
                function(i) {
                    self$contexts[[i]]
                }
            )
        },

        add_contact = function(adds, devs) {
            "Add the contacts specified in the list"
            for (i in adds) {
                if (length(self$contacts) >= params$max_number_contacts) {
                    break
                }
                if (length(devs[[i]]$contacts) < params$max_number_contacts) {
                    self$new_contact(i)
                    devs[[i]]$new_contact(self$id)
                }
            }
        },

        new_contact = function(add) {
            "Add a single new contact"
            self$contacts <- union(self$contacts, add)
        },

        set_trusts = function() {
            "Set up the trusts for the service providers in the network"
            self$trust <- rep(0, params$number_nodes)
            self$distrust <- rep(0, params$number_nodes)
            self$unknown <- rep(0, params$number_nodes)
            self$sp_trust <- 0
            self$sp_distrust <- 0
            self$sp_unknown <- 0
        },

        new_goal = function() {
            "Find a new location to head towards"
            while (all(self$current_goal == self$location) ||
                (self$domain == WATER &&
                    self$map[[1]]$get_tile(self$current_goal)[[1]]$terrain != WATER)) {
                self$current_goal <- round(runif(2, min = 1, max = self$map[[1]]$shape()))
            }
        },

        sp_trust_increment = function() {
            "Increment the trust count of the service provider"
            self$sp_trust <- self$sp_trust + 1
            # self$sp_trust <- 100
        },

        sp_distrust_increment = function() {
            "Increment the distrust count of the service provider"
            self$sp_distrust <- self$sp_distrust + 1
        },

        sp_unknown_increment = function() {
            "Increment the unknown count of the service provider"
            self$sp_unknown <- self$sp_unknown + 1
        },

        recieve_observation = function(obs) {
            "Receive a recommendation from the sender"
            if ((length(self$contexts[[obs$id_sender]]) / length(params$context_weights)) >=
                params$compression_factor) {
                w_context <- find_weighted_context(
                    c(self$contexts[[obs$id_sender]], obs$context)
                )
                self$stored_trusts[[obs$id_sender]] <- `if`(
                    obs$id_sender == self$id,
                    self$stored_trusts[[obs$id_sender]] <- direct_trust(
                        c(self$stored_trusts[[obs$id_sender]], obs$trust),
                        c(self$contexts[[obs$id_sender]], obs$context),
                        w_context
                    ),
                    self$stored_trusts[[obs$id_sender]] <- indirect_trust(
                        c(self$stored_trusts[[obs$id_sender]], obs$trust),
                        self$reputations[[obs$id_sender]],
                        c(self$contexts[[obs$id_sender]], obs$context),
                        find_weighted_context(self$contexts[[self$id]]),
                        w_context
                    )
                )
                self$contexts[[obs$id_sender]] <- w_context
            } else {
                cw_len <- length(params$context_weights)
                i <- `if`(
                    params$compression_factor < Inf,
                    length(self$stored_trusts[[obs$id_sender]]) + 1,
                    params$time_now
                )
                self$contexts[[obs$id_sender]][
                    get_context_index(i)
                ] <- obs$context
                self$stored_trusts[[obs$id_sender]][[i]] <- obs$trust
            }
        },

        should_consider_rec = function(id_sender, transaction_num) {
            "Check whether the recommendation should be considered"
            recced_trust <- self$stored_trusts[[id_sender]][transaction_num]
            return(
                recced_trust > (params$delta_a - params$trust_rep_adj_range) |
                    sapply(
                        seq_len(length(transaction_num)),
                        function(i) {
                            acceptable_rec(
                                self$cached_contexts[[id_sender]],
                                self$contexts[[id_sender]][get_context_index(transaction_num[[i]])],
                                recced_trust[[i]]
                            )
                        }
                    )
            )
        },

        move = function() {
            "Move towards the current goal"
            time_change <- params$time_now - self$time_last_moved
            old_signals <- self$get_signals()
            self$disconnect_all()
            movement_amount <- round(self$velocity * time_change)
            movement <- `if`(movement_amount > 0, 1:movement_amount, NULL)
            for (m in movement) {
                best_weight <- Inf
                best_loc <- NA
                best_tile <- NA
                for (i in (self$location[[1]] - 1):(self$location[[1]] + 1)) {
                    for (j in (self$location[[2]] - 1):(self$location[[2]] + 1)) {
                        loc <- c(i, j)
                        tile <- self$map[[1]]$get_tile(loc)
                        if (!all(loc == self$location) && length(tile)) {
                            tile <- tile[[1]]
                            cost <- `if`(
                                self$domain == AIR,
                                1,
                                `if`(
                                    self$domain == tile$terrain,
                                    1,
                                    2
                                )
                            )
                            weight <- cost + euc_dist(loc, self$current_goal)
                            if (weight < best_weight) {
                                best_weight <- weight
                                best_loc <- loc
                                best_tile <- tile
                            }
                        }
                    }
                }
                if (!all(is.na(best_loc))) {
                    self$map[[1]]$get_tile(self$location)[[1]]$rm_device(self$id)
                    self$location <- best_loc
                    best_tile$add_device(self)
                }
            }
            self$connect_all()
            self$retabulate_all(old_signals)
            self$velocity <- min(max(0, self$velocity + rnorm(1)), params$max_velocity)
            if (all(self$location == self$current_goal)) {
                self$new_goal()
            }
            self$time_last_moved <- params$time_now
        },

        disconnect_all = function() {
            "Disconnect from all base stations that this is currently connected to"
            for (signal in self$map[[1]]$get_tile(self$location)[[1]]$signals) {
                signal$disconnect(self)
            }
        },

        connect_all = function() {
            "Connect to all base stations currently in range of this"
            for (signal in self$map[[1]]$get_tile(self$location)[[1]]$signals) {
                signal$connect(self)
            }
        },

        retabulate_all = function(old_signals) {
            "After changing from being in one set of signals to another, make
            them recalculate their routing tables"
            if (self$has_signal()) {
                check_signals <- self$get_signals()
            } else {
                check_signals <- old_signals
            }
            for (signal in check_signals) {
                signal$retabulate(self)
            }
            for (signal in check_signals) {
                signal$finish_update()
            }
        },

        has_signal = function() {
            "Check whether this has signal"
            return(length(self$map[[1]]$get_tile(self$location)[[1]]$signals) > 0)
        },

        get_signals = function() {
            "Get the list of signals in range of this"
            return(self$map[[1]]$get_tile(self$location)[[1]]$signals)
        },

        transaction = function(devices) {
            "Perform a transaction with a service provider"
            normalized_c_target <- normalize(self$get_target_context())
            used_trust <- self$use_trust(normalized_c_target)
            if (used_trust > params$trust_rep_threshold - params$trust_rep_adj_range) {
                t_rs <- self$service_provider$provide_service()
                if (t_rs == TRUSTED) {
                    self$sp_trust_increment()
                } else if (t_rs == UNKNOWN) {
                    self$sp_unknown_increment()
                } else {
                    self$sp_distrust_increment()
                }
            } else {
                self$sp_distrust_increment()
            }
            prev_est_trust <- tail(self$estimated_trusts, 1)
            for (i in length(self$estimated_trusts):params$time_now) {
                if (i < params$time_now) {  # Might want to modify this to show decay
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

        use_trust = function(normalized_c_target) {
            "Calculate the trust value to use"
            rs_dir_trust <- self$find_direct_trust(normalized_c_target)
            return(
                `if`(
                    abs(rs_dir_trust$trust_comb) <=
                        (params$trust_rep_threshold + params$trust_rep_adj_range),
                    self$find_indirect_trust(normalized_c_target),
                    rs_dir_trust$trust_est
                )
            )
        },

        send_rec = function(devices) {
            "Send a recommendation to the contacts"
            self$emit_observation(
                Observation$new(
                    self$contexts[[self$id]][get_context_index(params$time_now)],
                    self$find_direct_trust(
                        self$contexts[[self$id]][get_context_index(params$time_now)]
                    )$trust_comb,
                    self$id
                ),
                devices
            )
        },

        get_target_context = function() {
            "Get the current target context"
            if (params$rand_context) {
                return(
                    c(
                        params$time_now,
                        runif(1, min = 0, max = params$max_capability),
                        euc_dist(
                            round(runif(2, min = 1, max = self$map[[1]]$size())),
                            self$service_provider$location
                        ),
                        runif(1, min = 0, max = params$max_velocity)
                    )
                )
            }
            return(
                c(
                    params$time_now,
                    self$capability,
                    euc_dist(self$location, self$service_provider$location),
                    self$velocity
                )
            )
        },

        find_direct_trust = function(normalized_c_target) {
            "Find the direct trust of the service provider"
            trust_evaled <- weighted_trust(
                compute_trust(self$sp_trust, self$sp_distrust, self$sp_unknown),
                self$sp_trust,
                self$sp_distrust,
                self$sp_unknown
            )
            valid_trusts <- !is.na(self$stored_trusts[[self$id]])
            valid_contexts <- !is.na(self$contexts[[self$id]])
            context_weighted <- find_weighted_context(self$contexts[[self$id]][valid_contexts])
            dir_trust <- direct_trust(
                c(self$stored_trusts[[self$id]][valid_trusts], trust_evaled),
                c(self$contexts[[self$id]][valid_contexts], context_weighted),
                context_weighted
            )
            # dir_trust <- max(-1, min(1, dir_trust))
            return(
                list(
                    trust_est = estimate_trust(
                        normalized_c_target,
                        context_weighted,
                        dir_trust
                    ),
                    trust_comb = dir_trust
                )
            )
        },

        find_indirect_trust = function(normalized_c_target) {
            "Find the indirect trust of the service provider"
            considerations <-self$get_considerations()
            all_contexts <- self$get_all_contexts(considerations)
            if (is.null(all_contexts)) {
                return(params$trust_new_contact)
            }
            context_weighted <- find_weighted_context(all_contexts[all_contexts >= 0])
            omega_weighted <- self$find_omega_w(context_weighted, considerations)
            num_part <- self$find_num_part(considerations)
            ind_trust <- sum(omega_weighted * num_part) / sum(omega_weighted)
            return(
                estimate_trust(
                    normalized_c_target,
                    context_weighted,
                    ind_trust
                )
            )
        },

        get_considerations = function(excludes=c()) {
            "Find which recommendations should be considered"
            lapply(
                1:params$number_nodes,
                function(i) {
                    `if`(
                        i %in% self$contacts & !i %in% excludes,
                        self$should_consider_rec(i, which(!is.na(self$stored_trusts[[i]]))),
                        FALSE
                    )
                }
            )
        },

        get_all_contexts = function(considerations) {
            "Get all of the context values that should be considered"
            unlist(
                lapply(
                    self$contacts,
                    function(i) {
                        self$contexts[[i]][!is.na(self$contexts[[i]])] *
                            ifelse(
                                as.vector(
                                    matrix(
                                        rep(considerations[[i]], length(params$context_weights)),
                                        nrow = length(params$context_weights),
                                        byrow = T
                                    )
                                ),
                                1,
                                -1
                            )
                    }
                )
            )
        },

        find_omega_w = function(context_weighted, considerations) {
            "Find a vector of omega for the given weighted contexts"
            unlist(
                lapply(
                    self$contacts,
                    function(i) {
                        return(
                            `if`(
                                self$reputations[[i]] < 0,
                                0, # Do not consider recs from trustees with -ve rep
                                omega(context_weighted, self$contexts[[i]][
                                    !is.na(self$contexts[[i]])
                                ]) *
                                    considerations[[i]]
                            )
                        )
                    }
                )
            )
        },

        find_num_part = function(considerations) {
            "Find the numerator exclusive part of the indirect trust combination"
            unlist(
                lapply(
                    self$contacts,
                    function(i) {
                        return(
                            `if`(
                                self$reputations[[i]] < 0,
                                0, # Do not consider recs from trustees with -ve rep
                                omega(self$cached_contexts[[i]], self$contexts[[i]][
                                        !is.na(self$contexts[[i]])
                                    ]) *
                                        self$reputations[[i]] *
                                        self$stored_trusts[[i]][!is.na(self$stored_trusts[[i]])] *
                                        # 10 *
                                        considerations[[i]]
                            )
                        )
                    }
                )
            )
        },

        performance_updates = function() {
            if (params$time_now > 1) {
                for (i in self$contacts) {
                    self$performance_update(i)
                }
            }
        },

        performance_update = function(id_sender) {
            "Update the stored performance of the observer"
            if (length(!is.na(self$stored_trusts[[id_sender]])) > 1) {
                sender_trust <- self$stored_trusts[[id_sender]][[params$time_now]]
                sender_context <- self$contexts[[id_sender]][get_context_index(params$time_now)]
                prev_time <- tail(which(!is.na(self$stored_trusts[[id_sender]])), 1)
                if (any(is.na(
                        c(
                            self$stored_trusts[[self$id]][prev_time],
                            self$stored_trusts[[self$id]][params$time_now]
                        )
                ))) {
                    context_trust_now <- self$get_ind_contexts_trust(params$time_now, id_sender)
                    context_trust_prev <- self$get_ind_contexts_trust(prev_time, id_sender)
                } else {
                    context_trust_now <- self$get_dir_contexts_trust(params$time_now)
                    context_trust_prev <- self$get_dir_contexts_trust(prev_time)
                }
                if (length(context_trust_now) > 0 && length(context_trust_prev) > 0) {
                    direct_trend <- trend_of_trust(
                        context_trust_prev$trust,
                        context_trust_now$trust,
                        context_trust_prev$context,
                        context_trust_now$context
                    )
                    indirect_trend <- trend_of_trust(
                        self$stored_trusts[[id_sender]][[prev_time]],
                        sender_trust,
                        self$contexts[[id_sender]][get_context_index(prev_time)],
                        sender_context
                    )
                    trends_diff <- abs(direct_trend - indirect_trend)
                    trends_max <- max(abs(direct_trend), abs(indirect_trend))
                    if (trends_diff < trends_max + params$trust_rep_adj_range) {
                        self$trust[[id_sender]] <- self$trust[[id_sender]] + 1
                    } else if (trends_diff <= max(trends_max, params$trend_threshold)) {
                        self$unknown[[id_sender]] <- self$unknown[[id_sender]] + 1
                    } else {
                        self$distrust[[id_sender]] <- self$distrust[[id_sender]] + 1
                    }
                }
            }
        },

        get_dir_contexts_trust = function(time) {
            "Get the direct context and trust from the time"
            return(
                list(
                    context = self$contexts[[self$id]][
                        get_context_index(time)
                    ],
                    trust = self$stored_trusts[[self$id]][[time]]
                )
            )
        },

        get_ind_contexts_trust = function(time, id_sender) {
            "Get the direct context and trust from the time, not including the sender"
                all_contexts <- unlist(
                    lapply(
                        setdiff(self$contacts, c(self$id, id_sender)),
                        function(i) {
                            cur_context <- self$contexts[[i]][
                                get_context_index(time)
                            ]
                            return(
                                `if`(
                                    any(is.na(cur_context)),
                                    NULL,
                                    cur_context
                                )
                            )
                        }
                    )
                )
                if (is.null(all_contexts)) {
                    return(list())
                }
                c_weighted <- find_weighted_context(all_contexts)
                considerations <- self$get_considerations(excludes=c(self$id, id_sender))
                omega_weighted <- self$find_omega_w(c_weighted, considerations)
                num_part <- self$find_num_part(considerations)
                t_comb <- sum(omega_weighted * num_part) / sum(omega_weighted)
                return(list(context = c_weighted, trust = t_comb))
        },

        combine_reps = function() {
            for (i in self$contacts) {
                self$combine_rep(i)
            }
        },

        combine_rep = function(id_sender) {
            "Find the new reputation for sender of recommendation"
            sender_context <- self$contexts[[id_sender]][get_context_index(params$time_now)]
            c_new <- find_weighted_context(
                c(self$cached_contexts[[id_sender]], sender_context)
            )
            if (params$time_now > 1) {
                self$reputations[[id_sender]] <- reputation_combination(
                    self$cached_contexts[[id_sender]],
                    sender_context,
                    c_new,
                    self$reputations[[id_sender]],
                    weighted_trust(
                        compute_trust(
                            self$trust[[id_sender]],
                            self$distrust[[id_sender]],
                            self$unknown[[id_sender]]
                        ),
                        self$trust[[id_sender]],
                        self$distrust[[id_sender]],
                        self$unknown[[id_sender]]
                    )
                )
                if (abs(self$reputations[[id_sender]]) <=
                    params$trust_rep_adj_range) {
                    self$reputations[[id_sender]] <- params$init_reputation
                }
            }
            self$cached_contexts[[id_sender]] <- c_new
        },

        emit_observation = function(observation, devices) {
            "send observation to all contacts"
            for (contact in self$contacts) {
                connection_data <- self$communicate(contact)
                if (connection_data[[1]] < Inf) {
                    # routed communication
                    connection_data[[2]]$recieve_observation(observation)
                } else if (euc_dist(
                    devices[[contact]]$location,
                    self$location
                ) <=
                    params$dev_signal_radius) {
                    # direct communication
                    devices[[contact]]$recieve_observation(observation)
                }
            }
        },

        communicate = function(contact_id) {
            "Communicate with a random contact"
            this_tile <- self$map[[1]]$get_tile(self$location)[[1]]
            best_signal <- 1
            for (i in seq_len(length(this_tile$signals))) {
                if (this_tile$signals[[i]]$table$hops[[contact_id]] <=
                    this_tile$signals[[best_signal]]$table$hops[[contact_id]]) {
                    best_signal <- i
                }
            }
            if (this_tile$signals[[best_signal]]$table$hops[[contact_id]] < Inf) {
                other_device <- this_tile$signals[[best_signal]]$find_device(contact_id)
            } else {
                other_device <- NULL
            }
            return(
                list(
                    this_tile$signals[[best_signal]]$table$hops[[contact_id]],
                    other_device
                )
            )
        }
    )
)
