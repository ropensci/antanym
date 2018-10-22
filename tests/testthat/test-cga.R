context("CGA-specific functionality")

test_that("preferred name selection works", {
    g <- an_read(cache = "session", simplified = FALSE)
    expect_equal((g %>% an_filter("^Ufs") %>% an_preferred(origin = c("Australia", "United States of America")))$origin, c("Australia", "United States of America", "United States of America", "United States of America"))
    expect_equal((g %>% an_filter("^Ufs") %>% an_preferred(origin = c("United States of America", "Australia")))$origin, rep("United States of America", 4))
})

test_that("an_filter behaves as expected", {
    ## filter by origin
    g <- an_read(cache = "session", simplified = FALSE)
    g2 <- an_filter(g, origin = "Bulgaria")
    expect_true(all(g2$country_name == "Bulgaria"))
    expect_true(all(g2$cga_source_gazetteer == "BGR"))
})

test_that("cga data integrity", {
    g <- an_read(cache = "session", simplified = FALSE)
    expect_true(all(c("gaz_id", "cga_source_gazetteer", "origin", "scar_common_id", "longitude", "latitude") %in% names(g)))
    expect_false(any(is.na(g$cga_source_gazetteer)))
    expect_false(any(duplicated(g$gaz_id)))
    expect_false(any(is.na(g$gaz_id)))
    expect_false(any(is.na(g$scar_common_id)))
    expect_false(any(is.na(g$longitude)))
    expect_false(any(is.na(g$latitude)))
    expect_false(any(is.na(g$origin)))
})
