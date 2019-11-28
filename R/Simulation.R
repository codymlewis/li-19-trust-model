#' @include ContextSetter.R
#' @include GoodMouther.R
#' @include BadMouther.R
#' @include ServiceProvider.R
#' @include Field.R
#' @include Device.R
#' @include Observer.R
NULL

#' Run the simulation in a console interface
#'
#' Simulate the trust model and the mobile network and iterate for an amount
#' of time
#' @keywords trust model simulate simulation run
#' @export run_simulation

run_simulation <- function(total_time,
                           map_filename = system.file(
                               "extdata", "map.csv",
                               package = "li19trustmodel"
                           )) {
    map_and_devices <- create_map_and_devices(map_filename)
    dir.create("images/maps", recursive = TRUE, showWarning = FALSE)
    img <- write_map(map_and_devices$map)
    cat("Performing transactions...\n")
    while (params$time_now <= total_time) {
        set_trusts(map_and_devices$devices)
        movements <- transact_and_move(map_and_devices$devices)
        img <- update_map(params$time_now, movements[[1]], movements[[2]], img, map_and_devices$map)
        cat_progress(
            params$time_now,
            total_time,
            prefix = sprintf("Time %d of %d", params$time_now, total_time)
        )
        params$increment_time()
    }
    cat("Plotting estimated trusts...\n")
    dir.create("images/plots", showWarning = FALSE)
    for (i in 1:params$number_nodes) {
        plot_estimated_trust(i, map_and_devices$devices)
        filename <- sprintf("images/plots/device-%d-estimated-trust.png", i)
        ggplot2::ggsave(file = filename, width = 7, height = 7, dpi = 320)
        cat_progress(
            i,
            params$number_nodes,
            prefix = sprintf("Device %d of %d", i, params$number_nodes),
            postfix = sprintf("Saved to %s", filename)
        )
    }
}


#' Run the simulation in a graphical interface
#'
#' Simulate the trust model and the mobile network and iterate
#' @keywords trust model simulate simulation run
#' @export run_gui

run_gui <- function(map_filename = system.file("extdata", "map.csv", package = "li19trustmodel")) {
    map_and_devices <- create_map_and_devices(map_filename)
    dir.create("images/maps", recursive = TRUE, showWarning = FALSE)
    dir.create("images/plots", recursive = TRUE, showWarning = FALSE)
    img <- write_map(map_and_devices$map)
    map_filename <- sprintf("images/maps/map-%d.png", params$time_now)
    cat("Performing transactions...\n")
    tt <- tcltk::tktoplevel()
    tcltk::tktitle(tt) <- "Li 2019 Trust Model"
    tcltk::tcl(
        "image",
        "create",
        "photo",
        "map",
        file = sprintf("images/maps/map-%d.png", params$time_now)
    )
    maplabel <- tcltk2::tk2label(tt, image = "map", compound = "image")
    tcltk::tkgrid(maplabel, row = "0", column = "0")
    timelabel <- tcltk2::tk2label(tt, text = sprintf("Current time: %d", params$time_now))
    tcltk::tkgrid(timelabel, row = "1", column = "0")
    filename <- tempfile(fileext = ".png")
    png(filename = filename, width = params$img_width, height = params$img_height)
    print(
        plot_estimated_trust(
            length(length(map_and_devices$devices)),
            map_and_devices$devices,
            title = "Estimated Trusts of the Observer"
        )
    )
    dev.off()
    tcltk::tcl("image", "create", "photo", "trustest", file = filename)
    trustlabel <- tcltk2::tk2label(tt, image = "trustest", compound = "image")
    tcltk::tkgrid(trustlabel, row = "0", column = "1")
    closebut <- tcltk2::tk2button(
        tt,
        text = "Save and Exit",
        command = function() {
            plot_estimated_trust(
                length(map_and_devices$devices),
                map_and_devices$devices,
                title = "Estimated Trusts of the Observer"
            )
            filename <- sprintf(
                "images/plots/device-%d-estimated-trust.png",
                length(map_and_devices$devices)
            )
            ggplot2::ggsave(file = filename, width = 7, height = 7, dpi = 320)
            cat(sprintf("Saved estimated trust plot to %s\n", filename))
            cat("Bye.\n")
            quit("no")
        }
    )
    tcltk::tkgrid(closebut, row = "1", column = "1")
    repeat {
        params$increment_time()
        set_trusts(map_and_devices$devices)
        movements <- transact_and_move(map_and_devices$devices)
        img <- update_map(
            params$time_now,
            movements[[1]],
            movements[[2]],
            img,
            map_and_devices$map
        )
        tcltk::tcl(
            "image",
            "create",
            "photo",
            "map",
            file = sprintf("images/maps/map-%d.png", params$time_now)
        )
        filename <- tempfile(fileext = ".png")
        png(filename = filename, width = params$img_width, height = params$img_height)
        print(
            plot_estimated_trust(
                length(map_and_devices$devices),
                map_and_devices$devices,
                title = "Estimated Trusts of the Observer"
            )
        )
        dev.off()
        tcltk::tcl("image", "create", "photo", "trustest", file = filename)
        tcltk::tkconfigure(timelabel, text = sprintf("Current time: %d", params$time_now))
    }
    tcltk::tkdestroy(tt)
}


write_map <- function(map, save = TRUE) {
    cat("Creating map image...\n")
    npixels <- params$img_width * params$img_height
    red <- matrix(0, nrow = params$img_height, ncol = params$img_width)
    green <- matrix(0, nrow = params$img_height, ncol = params$img_width)
    blue <- matrix(0, nrow = params$img_height, ncol = params$img_width)
    img <- array(
        c(red, green, blue),
        dim = c(params$img_height, params$img_width, 3)
    )
    width_factor <- ceiling(params$img_width / params$map_width)
    height_factor <- ceiling(params$img_height / params$map_height)
    for (i in 1:params$map_height) {
        for (j in 1:params$map_width) {
            cur_tile <- map$get_tile(c(i, j))[[1]]
            for (k in 1:height_factor) {
                for (l in 1:width_factor) {
                    img[
                        (i - 1) * height_factor + k,
                        (j - 1) * width_factor + l,
                    ] <- draw_map(cur_tile)
                }
            }
        }
        cat_progress(
            i,
            params$map_height,
            prefix = sprintf("Row %d of %d", i, params$map_height)
        )
    }
    if (save) {
        filename <- sprintf("images/maps/map-%d.png", params$time_now)
        png::writePNG(img, filename)
        cat(sprintf("Written %s\n", filename))
    }
    return(img)
}


update_map <- function(time, old_locs, new_locs, img, map, save = TRUE) {
    img <- update_map_locs(old_locs, img, map)
    img <- update_map_locs(new_locs, img, map)
    if (save) {
        filename <- sprintf("images/maps/map-%d.png", time)
        png::writePNG(img, filename)
    }
    return(img)
}


update_map_locs <- function(locs, img, map) {
    width_factor <- ceiling(params$img_width / params$map_width)
    height_factor <- ceiling(params$img_height / params$map_height)
    for (loc in locs) {
        cur_tile <- map$get_tile(loc)[[1]]
        for (i in 1:height_factor) {
            for (j in 1:width_factor) {
                img[
                    (loc[[1]] - 1) * height_factor + i, (loc[[2]] - 1)
                    * width_factor + j,
                ] <- draw_map(cur_tile)
            }
        }
    }
    return(img)
}


draw_map <- function(cur_tile) {
    result <- c(0, 0, 0)
    if (cur_tile$terrain == WATER) {
        result <- c(0.063, 0.612, 0.820)
    } else {
        result <- c(0.549, 0.761, 0.376)
    }
    if (cur_tile$signal_edge) {
        result <- c(0, 0, 0)
    }
    if (length(cur_tile$base_station)) {
        result <- c(0.2, 0.2, 0.2)
    }
    if (cur_tile$has_devices()) {
        dev_class <- class(cur_tile$get_first_dev())[[1]]
        if (grepl("Device", dev_class) || grepl("Observer", dev_class)) {
            result <- c(1, 0, 1)
        } else {
            result <- c(1, 0, 0)
        }
    }
    return(result)
}


create_map_and_devices <- function(map_filename) {
    sp <- ServiceProvider$new()
    map <- Field$new(read.csv(map_filename, header = F), T)
    cat("Creating devices...\n")
    devices <- lapply(
        seq_len(params$number_good_nodes),
        function(i) {
            cat_progress(
                i,
                params$number_nodes,
                prefix = sprintf("Device %d of %d", i, params$number_nodes)
            )
            return(Device$new(i, sp, map))
        }
    )
    for (i in seq_len(params$number_adversaries)) {
        cat_progress(
            params$number_good_nodes + i,
            params$number_nodes,
            prefix = sprintf("Device %d of %d", i, params$number_nodes)
        )
        dev_id <- params$number_good_nodes + i
        devices[[dev_id]] <- params$adversary_type$new(dev_id, sp, map)
    }
    i <- length(devices) + 1
    cat_progress(
        i,
        params$number_nodes,
        prefix = sprintf("Device %d of %d", i, params$number_nodes)
    )
    devices[[length(devices) + 1]] <- Observer$new(i, sp, map)
    lapply(
        1:params$number_good_nodes,
        function(i) {
            devices[[i]]$add_contact(
                sample(
                    setdiff(1:params$number_nodes, i),
                    params$contacts_per_node
                ),
                devices
            )
        }
    )
    devices[[length(devices)]]$add_contact(
        c(
            sample(1:params$number_good_nodes, params$contacts_per_node),
            (params$number_good_nodes + 1):(params$number_good_nodes + params$number_adversaries)
        ),
        devices
    )
    return(list(map = map, devices = devices))
}


set_trusts <- function(devices) {
    for (device in devices) {
        device$set_trusts()
    }
}


transact_and_move <- function(devices) {
    old_locs <- list()
    new_locs <- list()
    for (device in devices) {
        old_locs[[device$id]] <- device$location
        if (device$has_signal()) {
            amount_transactions <- params$min_trans:round(
                runif(1, min = params$min_trans, max = params$max_trans)
            )
            for (i in setdiff(amount_transactions, 0)) {
                device$transaction(devices)
            }
            if (length(setdiff(amount_transactions, 0)) >= 1) {
                device$send_rec(devices)
            }
        }
        device$move()
        new_locs[[device$id]] <- device$location
    }
    return(list(old_locs, new_locs))
}


plot_estimated_trust <- function(
                                 dev_id,
                                 devices,
                                 title = sprintf("Estimated Trusts of Device %d", dev_id)) {
    data <- data.frame(
        transactions = seq_len(length(devices[[dev_id]]$estimated_trusts)),
        estimated_trusts = devices[[dev_id]]$estimated_trusts
    )
    plt <- ggplot2::ggplot(data = data, ggplot2::aes(x = transactions, y = estimated_trusts)) +
        ggplot2::labs(
            title = title,
            x = "Time",
            y = "Estimated Trust",
            colour = NULL
        )# +
        # ggplot2::scale_y_continuous(limits = c(-1.1, 1.1))
    return(
        `if`(
            length(devices[[dev_id]]$estimated_trusts) > 1,
            plt + ggplot2::geom_line(colour = "blue"),
            plt + ggplot2::geom_point(colour = "blue")
        )
    )
}
