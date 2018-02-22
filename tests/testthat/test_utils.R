context("tests of general antanym utility functions")

test_that("list of gazetters is as expected",{
    expect_identical(an_gazetteers(),c("cga"))
})

