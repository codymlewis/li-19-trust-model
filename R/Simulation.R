#' Run the simulation
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
    estimated.trusts <- rep(0, total.time)
    cat("Performing transactions...\n")
    while (Params$time.now <= total.time) {
        for (device in devices) {
            if (device$has.signal()) {
                amount.transactions <- 0:round(runif(1, min=0, max=Params$transactions.per.time))
                for (i in setdiff(amount.transactions, 0)) {
                    device$transaction(Params$time.now)
                }
            }
            device$move(Params$time.now)
        }
        estimated.trusts[[Params$time.now]] <- tail(devices[[1]]$estimated.trusts, 1)
        cat.progress(
            Params$time.now,
            total.time,
            prefix=sprintf("Time %d of %d", Params$time.now, total.time)
        )
        Params$time.now <- Params$time.now + 1
    }
    data <- data.frame(
        transactions=1:total.time,
        estimated.trusts=estimated.trusts
    )
    ggplot2::ggplot(data=data, ggplot2::aes(x=transactions, y=estimated.trusts)) +
        ggplot2::geom_line(colour="blue") +
        ggplot2::labs(
            title="Estimated Trusts of Device 1",
            x="Time",
            y="Estimate Trust",
            colour=NULL
        )
    ggplot2::ggsave(file="estimated-trust.png", dpi=320)
}
