test_that("terrain assignment works", {
    expect_equal(Tile(LAND)$terrain, LAND)
})

test_that("device storing works", {
    Params$number.nodes <<- 200
    d <- Device(1, NULL)
    t <- Tile(LAND)
    t$add.device(d)
    expect_equal(t$objects[[d$id]]$id, d$id)
    t$rm.device(d$id)
    expect_false(t$has.devices())
})

test_that("base station storing works", {
    b <- BaseStation(1, 1)
    t <- Tile(LAND)
    t$add.base.station(b)
    expect_equal(c(1, 1), t$get.base.station()$location)
})

test_that("signal works", {
    b <- BaseStation(1, 1)
    t <- Tile(LAND)
    t$add.signal(b)
    expect_equal(c(1, 1), t$signals[[1]]$location)
})
