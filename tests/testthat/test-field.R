test_that("creation works", {
  params$map.width <<- 3
  params$map.height <<- 3
  map.data <- data.frame(
    rep(LAND, 3), rep(LAND, 3), rep(WATER, 3)
  )
  f <- Field(map.data)
  expect_equal(f$shape(), c(3, 3))
  params$map.width <<- 500
  params$map.height <<- 500
})
