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
    map <- Field(read.csv(map.filename))
    while (Params$time.now < total.time) {
        Params$time.now <- Params$time.now + 1
    }
}
