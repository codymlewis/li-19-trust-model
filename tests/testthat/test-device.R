is_equal <- function(...)
{
    return (isTRUE(all.equal(...)))
}

Params$map.width <<- 3
Params$map.height <<- 3
Params$number.nodes <<- 3
map.data <- data.frame(
    rep(LAND, 3), rep(LAND, 3), rep(WATER, 3)
)
f <- Field(map.data)
d <- Device(1, f, c(1, 1))

test_that("device creation works", {
    expect_equal(d$id, 1)
    expect_equal(d$last.rec.time, -Inf)
    expect_equal(d$time.last.moved, Params$time.now)
    expect_equal(d$trust, 0)
    expect_equal(d$distrust, 0)
    expect_equal(d$unknown, 0)
})


test_that("trust update works", {
    d$trust.increment(1)
    expect_equal(d$trust, 1)
})


test_that("distrust update works", {
    d$distrust.increment(1)
    expect_equal(d$distrust, 1)
})


test_that("unknown update works", {
    d$unknown.increment(1)
    expect_equal(d$unknown, 1)
})


test_that("recieving observations works", {
    context = c(1, 1, 1, 1)
    obs <- Observation(context, 1, 1)
    d$recieve.observation(obs)
    expect_equal(d$recommendations[[1]]$context, obs$context)
})


test_that("has signal works", {
    expect_equal(d$has.signal(f), T)
})


test_that("goal setting works", {
    expect_true(all(d$current.goal >= c(1, 1)))
    expect_true(all(d$current.goal <= f$shape()))
    d$new.goal()
    expect_true(all(d$current.goal >= c(1, 1)))
    expect_true(all(d$current.goal <= f$shape()))
})


test_that("moving works", {
    init.goal <- c(3, 2)
    init.velocity <- 1
    init.basestation.id <- d$get.signals(f)[[1]]$location
    init.tile.node.count <- length(f$get.tile(c(1, 1))[[1]]$objects)
    init.next.tile.node.count <- length(f$get.tile(c(2, 2))[[1]]$objects)
    d$current.goal <- init.goal
    d$velocity <- init.velocity
    Params$time.now <- Params$time.now + 1
    d$move(f, Params$time.now)
    expect_equal(d$location, c(2, 2))
    # expect_equal(length(f$get.tile(c(2, 2))[[1]]$objects), init.next.tile.node.count + 1)
    # expect_equal(length(f$get.tile(c(1, 1))[[1]]$objects), init.tile.node.count - 1)
    expect_equal(d$time.last.moved, Params$time.now)
    expect_equal(init.basestation.id, d$get.signals(f)[[1]]$location)
    expect_false(is_equal(d$velocity, init.velocity))
    Params$time.now <- Params$time.now + 1
    d$velocity <- init.velocity
    d$move(f, Params$time.now)
    expect_equal(d$location, c(3, 2))
    expect_equal(d$time.last.moved, Params$time.now)
    expect_equal(init.basestation.id, d$get.signals(f)[[1]]$location)
    expect_false(is_equal(d$current.goal, init.goal))
    expect_false(is_equal(d$velocity, init.velocity))
})


test_that("transaction works", {

})
