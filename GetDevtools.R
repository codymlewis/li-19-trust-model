#!/usr/bin/env Rscript

if (!require(devtools)) {
    r = getOption("repos")
    r["CRAN"] = "https://cran.csiro.au/"
    options(repos = r)
    rm(r)

    install.packages("devtools")
}
