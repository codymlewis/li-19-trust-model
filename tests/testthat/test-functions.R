test_that("Euclidean distance works", {
    expect_equal(sqrt(2), euc.dist(c(0, 0), c(1, 1)))
    expect_equal(2, euc.dist(c(0, 0), c(1, sqrt(3))))
})
