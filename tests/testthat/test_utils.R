context("tests of general antanym utility functions")

test_that("list of gazetteers is as expected",{
    expect_identical(an_gazetteers(), c("CGA"))
})

