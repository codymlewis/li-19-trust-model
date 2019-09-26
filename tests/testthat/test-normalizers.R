test_that("time normalizer works", {
    time <- 5
    expect_that(
        time / Params$time.now,
        equals(normalize.time(time))
    )
})


test_that("capability normalizer works", {
    cap <- 70
    expect_that(
        1 - (cap / Params$max.capability),
        equals(normalize.capability(cap))
    )
})


test_that("location normalizer works", {
    loc <- 30
    expect_that(
        1 - (loc / sqrt(Params$map.width**2 + Params$map.height**2)),
        equals(normalize.location(loc))
    )
})


test_that("velocity normalizer works", {
    vel <- 5
    expect_that(
        1 - (vel / Params$max.velocity),
        equals(normalize.velocity(vel))
    )
})


test_that("context normalizer works", {
    time <- 5
    cap <- 70
    loc <- 30
    vel <- 5
    expect_that(
        c(
            time / Params$time.now,
            1 - (cap / Params$max.capability),
            1 - (loc / sqrt(Params$map.width**2 + Params$map.height**2)),
            1 - (vel / Params$max.velocity)
        ),
        equals(normalize(c(time, cap, loc, vel)))
    )
})
