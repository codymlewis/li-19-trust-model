test_that("creation works", {
    Params$map.width <<- 3
    Params$map.height <<- 3
    map.data <- data.frame(
        rep(LAND, 3), rep(LAND, 3), rep(WATER, 3)
    )
    f <- Field(map.data)
    expect_equal(f$shape(), c(3, 3))
    Params$map.width <<- 500
    Params$map.height <<- 500
})
