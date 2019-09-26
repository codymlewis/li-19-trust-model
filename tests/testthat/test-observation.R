test_that("fields works", {
    context <- c(5, 70, 30, 5)
    trust <- 1
    id.sender <- 1
    obs <- Observation(context, trust, id.sender)
    expect_equal(normalize(context), obs$context)
    expect_equal(trust, obs$trust)
    expect_equal(id.sender, obs$id.sender)
})


test_that("Time update works", {
    context <- c(5, 70, 30, 5)
    trust <- 1
    id.sender <- 1
    obs <- Observation(context, trust, id.sender)
    prev.time <- obs$context[[1]]
    prev.time.now <- Params$time.now
    Params$time.now <- Params$time.now + 1
    obs$update.time()
    expect_equal(prev.time / prev.time.now, obs$context[[1]])
})
