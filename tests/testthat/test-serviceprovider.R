test_that("provide service works", {
    sp <- ServiceProvider()
    expect_equal(TRUE, sp$provide.service())
})
