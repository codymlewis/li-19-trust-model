#!/usr/bin/env Rscript

Rscript r = getOption("repos")
r["CRAN"] = "https://cran.csiro.au/"
options(repos = r)
rm(r)
install.packages(c("testthat"))

