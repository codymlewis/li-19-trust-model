test_that("provide service works", {
    sp <- ServiceProvider()
    expect_equal(TRUSTED, sp$provide_service())
})
