test_that("terrain assignment works", {
    expect_equal(Tile(LAND)$terrain, LAND)
})

test_that("device storing works", {
    params$number_nodes <<- 200
    sp <- ServiceProvider()
    d <- Device(1, sp, NULL)
    t <- Tile(LAND)
    t$add_device(d)
    expect_equal(t$objects[[d$id]]$id, d$id)
    t$rm_device(d$id)
    expect_false(t$has_devices())
})

test_that("base station storing works", {
    b <- BaseStation(1, 1)
    t <- Tile(LAND)
    t$add_base_station(b)
    expect_equal(c(1, 1), t$get_base_station()$location)
})

test_that("signal works", {
    b <- BaseStation(1, 1)
    t <- Tile(LAND)
    t$add_signal(b, F)
    expect_equal(c(1, 1), t$signals[[1]]$location)
})
