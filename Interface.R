#!/usr/bin/env Rscript

library(li19trustmodel)

main <- function() {
    run_simulation(2, config="inst/extdata/params.json")
    quit("no")
}

main()
