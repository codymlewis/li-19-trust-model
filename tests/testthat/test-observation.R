test_that("fields works", {
    context <- c(5, 70, 30, 5)
    trust <- 1
    id.sender <- 1
    obs <- Observation(context, params$time.now, trust, id.sender)
    expect_equal(context, obs$context)
    expect_equal(trust, obs$trust)
    expect_equal(id.sender, obs$id.sender)
})


test_that("Time update works", {
    context <- c(5, 70, 30, 5)
    trust <- 1
    id.sender <- 1
    obs <- Observation(context, params$time.now, trust, id.sender)
    params$increment.time()
    obs$update.time()
    expect_equal(obs$time / params$time.now, obs$context[[1]])
})
