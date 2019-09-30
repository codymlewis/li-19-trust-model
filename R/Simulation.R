#' Run the simulation in a console interface
#'
#' Simulate the trust model and the mobile network and iterate for an amount
#' of time
#' @keywords trust model simulate simulation run
#' @include Params.R
#' @include ServiceProvider.R
#' @include Field.R
#' @export run.simulation

run.simulation <- function(total.time,
                           map.filename=system.file("extdata", "map.csv", package="li19trustmodel"))
{
    map.and.devices <- create.map.and.devices(map.filename)
    dir.create("images/maps", recursive=TRUE, showWarning=FALSE)
    img <- write.map(map.and.devices$map)
    cat("Performing transactions...\n")
    while (Params$time.now <= total.time) {
        movements <- transact.and.move(map.and.devices$devices, map.and.devices$map, Params$time.now)
        img <- update.map(Params$time.now, movements[[1]], movements[[2]], img, map.and.devices$map)
        cat.progress(
            Params$time.now,
            total.time,
            prefix=sprintf("Time %d of %d", Params$time.now, total.time)
        )
        Params$time.now <- Params$time.now + 1
    }
    cat("Plotting estimated trusts...\n")
    dir.create("images/plots", showWarning=FALSE)
    for (i in 1:Params$number.nodes) {
        plot.estimated.trust(i, map.and.devices$devices)
        filename <- sprintf("images/plots/device-%d-estimated-trust.png", i)
        ggplot2::ggsave(file=filename, width=7, height=7, dpi=320)
        cat.progress(
            i,
            Params$number.nodes,
            prefix=sprintf("Device %d of %d", i, Params$number.nodes),
            postfix=sprintf("Saved to %s", filename)
        )
    }
}


write.map <- function(map)
{
    cat("Creating map image...\n")
    npixels <- Params$img.width * Params$img.height
    red <- matrix(0, nrow=Params$img.height, ncol=Params$img.width)
    green <- matrix(0, nrow=Params$img.height, ncol=Params$img.width)
    blue <- matrix(0, nrow=Params$img.height, ncol=Params$img.width)
    img <- array(c(red, green, blue), dim=c(Params$img.height, Params$img.width, 3))
    width.factor <- ceiling(Params$img.width / Params$map.width)
    height.factor <- ceiling(Params$img.height / Params$map.height)
    for (i in 1:Params$map.height) {
        for (j in 1:Params$map.width) {
            cur.tile <- map$get.tile(c(i, j))[[1]]
            for (k in 1: height.factor) {
                for (l in 1:width.factor) {
                    img[(i - 1) * height.factor + k, (j - 1) * width.factor + l,] <- draw.map(cur.tile)
                }
            }

        }
        cat.progress(
            i,
            Params$map.height,
            prefix=sprintf("Row %d of %d", i, Params$map.height)
        )
    }
    filename <- sprintf("images/maps/map-%d.png", Params$time.now)
    png::writePNG(img, filename)
    cat(sprintf("Written %s\n", filename))
    return (img)
}


update.map <- function(time, old.locs, new.locs, img, map)
{
    img <- update.map.locs(old.locs, img, map)
    img <- update.map.locs(new.locs, img, map)
    filename <- sprintf("images/maps/map-%d.png", time)
    png::writePNG(img, filename)
    return (img)
}


update.map.locs <- function(locs, img, map)
{
    width.factor <- ceiling(Params$img.width / Params$map.width)
    height.factor <- ceiling(Params$img.height / Params$map.height)
    for (loc in locs) {
        cur.tile <- map$get.tile(loc)[[1]]
        for (i in 1:height.factor) {
            for (j in 1:width.factor) {
                img[(loc[[1]] - 1) * height.factor + i, (loc[[2]] - 1) * width.factor  + j,] <- draw.map(cur.tile)
            }
        }
    }
    return (img)
}


draw.map <- function(cur.tile)
{
    result <- c(0, 0, 0)
    if (cur.tile$terrain == WATER) {
        result <- c(0, 0, 1)
    } else {
        result <- c(0, 1, 0)
    }
    if (length(cur.tile$base.station)) {
        result <- c(0, 0, 0)
    }
    if (cur.tile$has.devices()) {
        result <- c(1, 0, 1)
    }
    return (result)
}


create.map.and.devices <- function(map.filename)
{
    sp <- ServiceProvider()
    map <- Field(read.csv(map.filename, header=F), T)
    cat("Creating devices...\n")
    devices <- lapply(
        1:Params$number.nodes,
        function(i) {
            cat.progress(
                i,
                Params$number.nodes,
                prefix=sprintf("Device %d of %d", i, Params$number.nodes)
            )
            return (Device(i, map))
        }
    )
    return (list(map=map, devices=devices))
}


transact.and.move <- function(devices, map, time)
{
    old.locs <- list()
    new.locs <- list()
    for (device in devices) {
        old.locs[[device$id]] <- device$location
        if (device$has.signal(map)) {
            amount.transactions <- 0:round(runif(1, min=0, max=Params$transactions.per.time))
            for (i in setdiff(amount.transactions, 0)) {
                device$transaction(time, map)
            }
        }
        device$move(map, time)
        new.locs[[device$id]] <- device$location
    }
    return (list(old.locs, new.locs))
}


plot.estimated.trust <- function(dev.id, devices)
{
    data <- data.frame(
        transactions=1:length(devices[[dev.id]]$estimated.trusts),
        estimated.trusts=devices[[dev.id]]$estimated.trusts
    )
    ggplot2::ggplot(data=data, ggplot2::aes(x=transactions, y=estimated.trusts)) +
        ggplot2::geom_line(colour="blue") +
        ggplot2::labs(
            title=sprintf("Estimated Trusts of Device %d", dev.id),
            x="Time",
            y="Estimate Trust",
            colour=NULL
        ) +
        ggplot2::scale_y_continuous(limits=c(-1.1, 1.1))
}
