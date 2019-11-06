#' @include Params.R
#' @include Functions.R
#' @include TrustModel.R
#' @include ServiceProvider.R
#' @include Observation.R
#' @include Normalizers.R
#' @include Field.R

Device <- setRefClass(
    "Device",
    fields = list(
        id = "numeric",
        contacts = "numeric",
        location = "numeric",
        current_goal = "numeric",
        capability = "numeric",
        velocity = "numeric",
        trust = "numeric",
        distrust = "numeric",
        unknown = "numeric",
        sp_trust = "numeric",
        sp_distrust = "numeric",
        sp_unknown = "numeric",
        domain = "numeric",
        reputations = "numeric",
        service_provider = "ServiceProvider",
        time_last_moved = "numeric",
        estimated_trusts = "numeric",
        map = "list",
        contexts = "list",
        stored_trusts = "list",
        cached_contexts = "list"
    ),

    methods = list(
        initialize = function(id, sp, map, loc = round(runif(2, min = 1, max = map_size))) {
            service_provider <<- sp
            if (!is.null(map)) {
                map_size <- map$shape()
                map <<- list(map)
                location <<- loc
                if (round(runif(1)) == 1) {
                    domain <<- AIR
                } else {
                    domain <<- map$get_tile(location)[[1]]$terrain
                }
                new_goal()
            } else {
                map_size <- c(params$map_width, params$map_height)
                location <<- loc
                map <<- list()
                domain <<- sample(c(AIR, LAND, WATER), 1)
            }
            id <<- id
            set_trusts()
            if (!is.null(map)) {
                map$get_tile(location)[[1]]$add_device(.self)
                for (signal in map$get_tile(location)[[1]]$signals) {
                    signal$connect(.self)
                }
            }
            velocity <<- runif(1, min = 0, max = params$max_velocity)
            capability <<- runif(1, min = 1, max = params$max_capability)
            reputations <<- rep(params$init_reputation, params$number_nodes)
            time_last_moved <<- params$time_now
            estimated_trusts <<- c(params$trust_new_contact)
            contexts <<- lapply(
                1:params$number_nodes,
                function(i) {
                    `if`(
                        i == id,
                        normalize(
                            c(
                                params$time_now,
                                capability,
                                euc_dist(location, service_provider$location),
                                velocity
                            )
                        ),
                        c(params$time_now, 0, 0, 0)
                    )
                }
            )
            stored_trusts <<- lapply(
                1:params$number_nodes,
                function(i) {
                    params$trust_new_contact
                }
            )
            reputations <<- sapply(
                1:params$number_nodes,
                function(i) {
                    `if`(
                        i == id,
                        params$rep_self,
                        params$init_reputation
                    )
                }
            )
            cached_contexts <<- lapply(
                1:params$number_nodes,
                function(i) {
                    contexts[[i]]
                }
            )
        },

        add_contact = function(adds, devs) {
            for (i in adds) {
                if (length(contacts) > params$max_number_contacts) {
                    break
                }
                if (length(devs[[i]]$contacts) < params$max_number_contacts) {
                    new_contact(i)
                    devs[[i]]$new_contact(id)
                }
            }
        },

        new_contact = function(add) {
            contacts <<- union(contacts, add)
        },

        set_trusts = function() {
            "Set up the trusts for the service providers in the network"
            trust <<- rep(0, params$number_nodes)
            distrust <<- rep(0, params$number_nodes)
            unknown <<- rep(0, params$number_nodes)
            sp_trust <<- 0
            sp_distrust <<- 0
            sp_unknown <<- 0
        },

        new_goal = function() {
            "Find a new location to head towards"
            while (all(current_goal == location) ||
                (domain == WATER && map[[1]]$get_tile(current_goal)[[1]]$terrain != WATER)) {
                current_goal <<- round(runif(2, min = 1, max = map[[1]]$shape()))
            }
        },

        trust_increment = function(contact_id) {
            "Increment the trust count of the service provider"
            trust[[contact_id]] <<- trust[[contact_id]] + 1
        },

        distrust_increment = function(contact_id) {
            "Increment the distrust count of the service provider"
            distrust[[contact_id]] <<- distrust[[contact_id]] + 1
        },

        unknown_increment = function(contact_id) {
            "Increment the unknown count of the service provider"
            unknown[[contact_id]] <<- unknown[[contact_id]] + 1
        },

        sp_trust_increment = function() {
            "Increment the trust count of the service provider"
            sp_trust <<- sp_trust + 1
        },

        sp_distrust_increment = function() {
            "Increment the distrust count of the service provider"
            sp_distrust <<- sp_distrust + 1
        },

        sp_unknown_increment = function() {
            "Increment the unknown count of the service provider"
            sp_unknown <<- sp_unknown + 1
        },

        recieve_observation = function(obs) {
            "Receive a recommendation from the sender"
            performance_update(obs)
            combine_rep(obs)
            if ((length(contexts[[obs$id_sender]]) / length(params$context_weights)) >=
                params$compression_factor) {
                w_context <- find_weighted_context(
                    c(contexts[[obs$id_sender]], obs$context)
                )
                stored_trusts[[obs$id_sender]] <<- `if`(
                    obs$id_sender == id,
                    stored_trusts[[obs$id_sender]] <<- direct_trust(
                        c(stored_trusts[[obs$id_sender]], obs$trust),
                        c(contexts[[obs$id_sender]], obs$context),
                        w_context
                    ),
                    stored_trusts[[obs$id_sender]] <<- indirect_trust(
                        c(stored_trusts[[obs$id_sender]], obs$trust),
                        reputations[[obs$id_sender]],
                        c(contexts[[obs$id_sender]], obs$context),
                        find_weighted_context(contexts[[id]]),
                        w_context
                    )
                )
                contexts[[obs$id_sender]] <<- w_context
            } else {
                cw_len <- length(params$context_weights)
                i <- `if`(
                    params$compression_factor < Inf,
                    length(stored_trusts[[obs$id_sender]]) + 1,
                    params$time_now
                )
                contexts[[obs$id_sender]][
                    (cw_len * (i - 1) + 1):(cw_len * i)
                ] <<- obs$context
                stored_trusts[[obs$id_sender]][[i]] <<- obs$trust
            }
        },

        should_consider_rec = function(id_sender, transaction_num) {
            "Check whether the recommendation should be considered"
            recced_trust <- stored_trusts[[id_sender]][transaction_num]
            return(
                recced_trust > (params$delta_a - params$trust_rep_adj_range) |
                    sapply(
                        1:length(transaction_num),
                        function(i) {
                            acceptable_rec(
                                cached_contexts[[id_sender]],
                                contexts[[id_sender]][get_context_index(transaction_num[[i]])],
                                recced_trust[[i]]
                            )
                        }
                    )
            )
        },

        move = function() {
            "Move towards the current goal"
            time_change <- params$time_now - time_last_moved
            old_signals <- get_signals()
            disconnect_all()
            movement_amount <- round(velocity * time_change)
            movement <- `if`(movement_amount > 0, 1:movement_amount, NULL)
            for (m in movement) {
                best_weight <- Inf
                best_loc <- NA
                best_tile <- NA
                for (i in (location[[1]] - 1):(location[[1]] + 1)) {
                    for (j in (location[[2]] - 1):(location[[2]] + 1)) {
                        loc <- c(i, j)
                        tile <- map[[1]]$get_tile(loc)
                        if (!all(loc == location) && length(tile)) {
                            tile <- tile[[1]]
                            cost <- `if`(
                                domain == AIR,
                                1,
                                `if`(
                                    domain == tile$terrain,
                                    1,
                                    2
                                )
                            )
                            weight <- cost + euc_dist(loc, current_goal)
                            if (weight < best_weight) {
                                best_weight <- weight
                                best_loc <- loc
                                best_tile <- tile
                            }
                        }
                    }
                }
                if (!all(is.na(best_loc))) {
                    map[[1]]$get_tile(location)[[1]]$rm_device(id)
                    location <<- best_loc
                    best_tile$add_device(.self)
                }
            }
            connect_all()
            retabulate_all(old_signals)
            velocity <<- min(max(0, velocity + rnorm(1)), params$max_velocity)
            if (all(location == current_goal)) {
                new_goal()
            }
            time_last_moved <<- params$time_now
        },

        disconnect_all = function() {
            "Disconnect from all base stations that this is currently connected to"
            for (signal in map[[1]]$get_tile(location)[[1]]$signals) {
                signal$disconnect(.self)
            }
        },

        connect_all = function() {
            "Connect to all base stations currently in range of this"
            for (signal in map[[1]]$get_tile(location)[[1]]$signals) {
                signal$connect(.self)
            }
        },

        retabulate_all = function(old_signals) {
            "After changing from being in one set of signals to another, make
            them recalculate their routing tables"
            if (has_signal()) {
                check_signals <- get_signals()
            } else {
                check_signals <- old_signals
            }
            for (signal in check_signals) {
                signal$retabulate(.self)
            }
            for (signal in check_signals) {
                signal$finish_update()
            }
        },

        has_signal = function() {
            "Check whether this has signal"
            return(length(map[[1]]$get_tile(location)[[1]]$signals) > 0)
        },

        get_signals = function() {
            "Get the list of signals in range of this"
            return(map[[1]]$get_tile(location)[[1]]$signals)
        },

        transaction = function(devices) {
            "Perform a transaction with a service provider"
            normalized_c.target <- normalize(get_target_context())
            rs_dir_trust <- find_direct_trust(normalized_c.target)
            used_trust <- `if`(
                abs(rs_dir_trust$trust_comb) <=
                    (params$trust_rep_threshold + params$trust_rep_adj_range),
                find_indirect_trust(normalized_c.target),
                rs_dir_trust$trust_est
            )
            if (used_trust > params$trust_rep_threshold - params$trust_rep_adj_range) {
                t_rs <- service_provider$provide_service()
                if (t_rs == TRUSTED) {
                    sp_trust_increment()
                } else if (t_rs == UNKNOWN) {
                    sp_unknown_increment()
                } else {
                    sp_distrust_increment()
                }
            }
            prev_est_trust <- tail(estimated_trusts, 1)
            for (i in length(estimated_trusts):params$time_now) {
                if (i < params$time_now) {
                    estimated_trusts[[i]] <<- prev_est_trust
                } else {
                    estimated_trusts[[i]] <<- used_trust
                }
            }
            recieve_observation(rs_dir_trust$obs)
        },

        send_rec = function(devices) {
            emit_observation(
                Observation(
                    contexts[[id]][get_context_index(params$time_now)],
                    weighted_trust(
                        compute_trust(sp_trust, sp_distrust, sp_unknown),
                        sp_trust,
                        sp_distrust,
                        sp_unknown
                    ),
                    id
                )
            )
        },

        get_target_context = function() {
            "Get the current target context"
            return(
                c(
                    params$time_now,
                    capability,
                    euc_dist(location, service_provider$location),
                    velocity
                )
            )
        },

        find_direct_trust = function(normalized_c.target) {
            "Find the direct trust of the service provider"
            trust_evaled <- weighted_trust(
                compute_trust(sp_trust, sp_distrust, sp_unknown),
                sp_trust,
                sp_distrust,
                sp_unknown
            )
            valid_trusts <- !is.na(stored_trusts[[id]])
            valid_contexts <- !is.na(contexts[[id]])
            context_weighted <- find_weighted_context(contexts[[id]][valid_contexts])
            dir_trust <- direct_trust(
                c(stored_trusts[[id]][valid_trusts], trust_evaled),
                c(contexts[[id]][valid_contexts], context_weighted),
                context_weighted
            )
            return(
                list(
                    obs = Observation(
                        normalized_c.target,
                        trust_evaled,
                        id
                    ),
                    trust_est = estimate_trust(
                        normalized_c.target,
                        context_weighted,
                        dir_trust
                    ),
                    trust_evaled = trust_evaled,
                    trust_comb = dir_trust
                )
            )
        },

        find_indirect_trust = function(normalized_c.target) {
            "Find the indirect trust of the service provider"
            considerations <- lapply(
                1:params$number_nodes,
                function(i) {
                    `if`(
                        i %in% contacts,
                        should_consider_rec(i, which(!is.na(stored_trusts[[i]]))),
                        FALSE
                    )
                }
            )
            all_contexts <- unlist(
                lapply(
                    contacts,
                    function(i) {
                        contexts[[i]][!is.na(contexts[[i]])] *
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
            if (is.null(all_contexts)) {
                return(params$trust_new_contact)
            }
            context_weighted <- find_weighted_context(all_contexts[all_contexts >= 0])
            omega_weighted <- unlist(
                lapply(
                    contacts,
                    function(i) {
                        return(
                            `if`(
                                reputations[[i]] < 0,
                                0, # Do not consider recs from trustees with -ve rep
                                omega(context_weighted, contexts[[i]][!is.na(contexts[[i]])]) *
                                    considerations[[i]]
                            )
                        )
                    }
                )
            )
            num_part <- unlist(
                lapply(
                    contacts,
                    function(i) {
                        omega(cached_contexts[[i]], contexts[[i]][!is.na(contexts[[i]])]) *
                            reputations[[i]] *
                            stored_trusts[[i]][!is.na(stored_trusts[[i]])] *
                            considerations[[i]]
                    }
                )
            )
            ind_trust <- sum(omega_weighted * num_part) / sum(omega_weighted)
            return(
                estimate_trust(
                    normalized_c.target,
                    context_weighted,
                    ind_trust
                )
            )
        },

        performance_update = function(obs) {
            "Update the stored performance of the observer"
            if (length(!is.na(stored_trusts[[obs$id_sender]])) > 1) {
                context_trust_now <- get_contexts_trust_ex_id(
                    params$time_now, obs$id_sender
                )
                prev_time <- tail(which(!is.na(stored_trusts[[obs$id_sender]]))[[1]])
                context_trust_prev <- get_contexts_trust_ex_id(
                    prev_time, obs$id_sender
                )
                if (length(context_trust_now) > 0 && length(context_trust_prev) > 0) {
                    direct_trend <- trend_of_trust(
                        context_trust_prev$trust,
                        context_trust_now$trust,
                        context_trust_prev$context,
                        context_trust_now$context
                    )
                    indirect_trend <- trend_of_trust(
                        stored_trusts[[obs$id_sender]][[prev_time]],
                        obs$trust,
                        contexts[[obs$id_sender]][get_context_index(prev_time)],
                        obs$context
                    )
                    trends_diff <- abs(direct_trend - indirect_trend)
                    trends_max <- max(abs(direct_trend), abs(indirect_trend))
                    if (trends_diff < trends_max) {
                        trust[[obs$id_sender]] <<- trust[[obs$id_sender]] + 1
                    } else if (trends_diff <= max(trends_max, params$trend_threshold)) {
                        unknown[[obs$id_sender]] <<- unknown[[obs$id_sender]] + 1
                    } else {
                        distrust[[obs$id_sender]] <<- distrust[[obs$id_sender]] + 1
                    }
                }
            }
        },

        get_contexts_trust_ex_id = function(time, id_sender) {
            "Get the contexts and trust from a particular time excluding the id"
            if (!is.na(stored_trusts[[id]][time])) {
                return(
                    list(
                        context = tail(contexts[[id]], length(params$context_weights)),
                        trust = stored_trusts[[id]][[time]]
                    )
                )
            } else {
                all_contexts <- unlist(
                    lapply(
                        setdiff(contacts, c(id, id_sender)),
                        function(i) {
                            cur_context <- contexts[[i]][get_context_index(time)]
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
                t_comb <- sum(
                    unlist(
                        lapply(
                            setdiff(contacts, c(id, id_sender)),
                            function(i) {
                                cur_context <- contexts[[i]][get_context_index(time)]
                                if (is.na(stored_trusts[[i]][time])) {
                                    return(NULL)
                                }
                                ind_trust <- indirect_trust(
                                    stored_trusts[[i]][[time]],
                                    reputations[[i]],
                                    cur_context,
                                    c_weighted,
                                    cached_contexts[[i]]
                                )
                                return(`if`(is.na(ind_trust), NULL, ind_trust))
                            }
                        )
                    )
                )
                return(list(context = c_weighted, trust = t_comb))
            }
        },

        combine_rep = function(obs) {
            "Find the new reputation for sender of recommendation"
            c_new <- find_weighted_context(
                c(cached_contexts[[obs$id_sender]], obs$context)
            )
            reputations[[obs$id_sender]] <<- reputation_combination(
                cached_contexts[[obs$id_sender]],
                obs$context,
                c_new,
                reputations[[obs$id_sender]],
                weighted_trust(
                    compute_trust(
                        trust[[obs$id_sender]],
                        distrust[[obs$id_sender]],
                        unknown[[obs$id_sender]]
                    ),
                    trust[[obs$id_sender]],
                    distrust[[obs$id_sender]],
                    unknown[[obs$id_sender]]
                )
            )
            if (abs(reputations[[obs$id_sender]]) <= params$trust_rep_adj_range) {
                reputations[[obs$id_sender]] <<- params$init_reputation
            }
            cached_contexts[[obs$id_sender]] <<- c_new
        },

        emit_observation = function(observation, devices) {
            "send observation to all contacts"
            for (contact in contacts) {
                connection_data <- communicate(contact)
                if (connection_data[[1]] < Inf) {
                    # routed communication
                    connection_data[[2]]$recieve_observation(observation)
                } else if (euc_dist(devices[[contact]]$location, location) <=
                    params$dev_signal_radius) {
                    # direct communication
                    devices[[contact]]$recieve_observation(observation)
                }
            }
        },

        communicate = function(contact_id) {
            "Communicate with a random contact"
            this_tile <- map[[1]]$get_tile(location)[[1]]
            best_signal <- 1
            for (i in 1:length(this_tile$signals)) {
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
