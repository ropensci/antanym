context("antanym data structures")

g <- an_read(cache = "session")

test_that("gaz_id is present and unique", {
    ## some functions rely on gaz_id, check that it's present and unique
    expect_true("gaz_id" %in% names(g))
    expect_false(any(duplicated(g$gaz_id)))
})

