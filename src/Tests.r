#!/usr/bin/env Rscript

library('testthat')

source('TrustModel.r')
source('Device.r')


test <- function(fun)
{
    cat("\nTest ")
    cat(sys.call()[[2]])
    cat(" time taken:\n")
    print(system.time(fun()))
    cat("Passed.\n")
}


case.probability <- function()
{
    a <- 5
    b <- 3
    c <- 2
    expect_that(compute.probability(a, b, c), equals(6 / 13))
}


case.ent.unit <- function()
{
    probability <- 1:5
    expect_that(
        ent.unit(probability),
        equals(
            c(0, 1.2618595071429148, 3, 5.047438028571659, 7.3248676035896345)
        )
    )
}


case.compute.entropy <- function()
{
    probability <- 1:5
    expect_that(compute.entropy(probability), equals(-16.63416513930421))
}


case.ent.unit.div <- function()
{
    probability <- 1:5
    expect_that(
        ent.unit.div(probability, 2),
        equals(
            c(-0.6309297535714574, 0, 1.1072107392856276, 2.5237190142858297, 4.170218835732348)
        )
    )

}


case.compute.trust <- function()
{
    a <- 5
    b <- 3
    c <- 2
    expect_that(compute.trust(a, b, c), equals(0.03203451826247661))
    expect_that(compute.trust(b, a, c), equals(-0.03203451826247661))
    expect_that(compute.trust(b, c, a), equals(-0.0013648071755567592))
    expect_that(compute.trust(a, c, b), equals(0.03705298))
    expect_that(compute.trust(c, a, b), equals(-0.03705298))
}


case.weighted.avg.context <- function()
{
    expect_that(
        weighted.avg.context(0.8, c(0.2, 0.3, 0.1, 0.5, 0.6)),
        equals(0.34769794209818133)
    )
}


case.context.distance <- function()
{
    expect_that(
        context.distance(
            c(0.3, 0.2, 0.4, 0.1),
            c(0.2, 0.3, 0.1, 0.5),
            c(0.3, 0.5, 0.6, 0.1)
        ),
        equals(0.3563705936241092)
    )
}


case.estimate.trust <- function()
{
    expect_that(
        estimate.trust(
            c(0.3, 0.2, 0.4, 0.1),
            c(0.2, 0.3, 0.1, 0.5),
            c(0.3, 0.5, 0.6, 0.1),
            c(0.7, 0.5, 0.5, 0.7),
            1,
            0.1
        ),
        equals(0.1167438402791883)
    )
    expect_that(
        estimate.trust(
            c(0.3, 0.2, 0.4, 0.1),
            c(0.2, 0.3, 0.1, 0.5),
            c(0.3, 0.5, 0.6, 0.1),
            c(0.7, 0.5, 0.5, 0.7),
            1,
            -0.1
        ),
        equals(-0.09330239773694914)
    )
}


case.delta <- function() {
    expect_that(
        delta(
            c(0.3, 0.2, 0.4, 0.1),
            c(0.2, 0.3, 0.1, 0.5),
            c(0.3, 0.5, 0.6, 0.1),
            1,
            T
        ),
        equals(c(0.029999999999999992, 0.04000000000000001, 0.2, 0.04))
    )
}


case.weighted.trust <- function()
{
    a <- 5
    b <- 3
    c <- 2
    expect_that(
        weighted.trust(0.1, a, b, c, 0.3, 0.3, 0.8),
        equals(0.03)
    )
    expect_that(
        weighted.trust(0.1, b, c, a, 0.3, 0.3, 0.8),
        equals(0.03)
    )
    expect_that(
        weighted.trust(0.1, c, a, a, 0.3, 0.3, 0.8),
        equals(0.08)
    )
}


case.direct.trust <- function()
{
    expect_that(
        direct.trust(
            0.1,
            c(0.3, 0.2, 0.4, 0.1),
            c(0.2, 0.3, 0.1, 0.5),
            c(0.3, 0.5, 0.6, 0.1),
            0.95,
            0.8
        ),
        equals(0.09774097906001301)
    )
}


case.indirect.trust <- function()
{
    expect_that(
        indirect.trust(
            0.1,
            0.1,
            c(0.3, 0.2, 0.4, 0.1),
            c(0.2, 0.3, 0.1, 0.5),
            c(0.3, 0.5, 0.6, 0.1),
            c(0.3, 0.5, 0.6, 0.1),
            0.95,
            0.8
        ),
        equals(0.009774097906001301)
    )
}


case.omega <- function()
{
    expect_that(
        omega(
            c(0.3, 0.2, 0.4, 0.1),
            c(0.2, 0.3, 0.1, 0.5),
            c(0.3, 0.5, 0.6, 0.1),
            0.95,
            0.8
        ),
        equals(0.9774097906001301)
    )
}


case.update.performance <- function()
{
    sp = Device()
    update.performance(
        sp,
        0.1,
        0.1,
        0.1,
        0.1,
        c(0.3, 0.2, 0.4, 0.1),
        c(0.2, 0.3, 0.1, 0.5),
        c(0.3, 0.5, 0.6, 0.1),
        c(0.2, 0.3, 0.1, 0.5),
        c(0.3, 0.5, 0.6, 0.1),
        0.95,
        0.8,
        0
    )
    expect_that(sp$trust, equals(1))
    update.performance(
        sp,
        0.1,
        0.8,
        0.1,
        -0.8,
        c(0.3, 0.2, 0.4, 0.1),
        c(0.2, 0.3, 0.1, 0.5),
        c(0.3, 0.5, 0.6, 0.1),
        c(0.2, 0.3, 0.1, 0.5),
        c(0.3, 0.5, 0.6, 0.1),
        0.95,
        0.8,
        0
    )
    expect_that(sp$distrust, equals(1))
    update.performance(
        sp,
        0.1,
        0.1,
        0.1,
        0.1,
        c(0.3, 0.2, 0.4, 0.1),
        c(0.2, 0.3, 0.1, 0.5),
        c(0.2, 0.3, 0.1, 0.5),
        c(0.2, 0.3, 0.1, 0.5),
        c(0.2, 0.3, 0.1, 0.5),
        0.95,
        0.8,
        0
    )
    expect_that(sp$unknown, equals(1))
}


case.trend.of.trust <- function()
{
    expect_that(
        trend.of.trust(
            0.1,
            0.1,
            c(0.3, 0.2, 0.4, 0.1),
            c(0.2, 0.3, 0.1, 0.5),
            c(0.3, 0.5, 0.6, 0.1),
            0.95,
            0.8
        ),
        equals(0.0022590209399869915)
    )
}


case.reputation.combination <- function()
{
    expect_that(
        reputation.combination(
            c(0.3, 0.2, 0.4, 0.1),
            c(0.2, 0.3, 0.1, 0.5),
            c(0.3, 0.5, 0.6, 0.1),
            0.1,
            0.1,
            0.8,
            0.95,
            0.8,
            0.1
        ),
        equals(0.1775286)
    )
    expect_that(
        reputation.combination(
            c(0.3, 0.2, 0.4, 0.1),
            c(0.2, 0.3, 0.1, 0.5),
            c(0.3, 0.5, 0.6, 0.1),
            0.1,
            -0.1,
            0.8,
            0.95,
            0.8,
            0.1
        ),
        equals(0.08656811)
    )
}


test.cases <- function()
{
    cat("-------------- Performing Tests --------------\n")
    test(case.probability)
    test(case.ent.unit)
    test(case.compute.entropy)
    test(case.ent.unit.div)
    test(case.compute.trust)
    test(case.weighted.avg.context)
    test(case.estimate.trust)
    test(case.delta)
    test(case.weighted.trust)
    test(case.direct.trust)
    test(case.indirect.trust)
    test(case.omega)
    test(case.update.performance)
    test(case.trend.of.trust)
    case.reputation.combination()
    cat("\n------------- Test Cases Passed --------------\n")
    cat("Build Succeeded\n")
}


test.cases()
