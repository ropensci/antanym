context("CGA-specific functionality")

g <- an_read(cache = "session")

test_that("preferred name selection works", {
    expect_equal((g %>% an_filter("^Ufs") %>% an_preferred(origin_country = c("Australia", "United States of America")))$country_name, c("Australia", "United States of America", "United States of America", "United States of America"))
    expect_equal((g %>% an_filter("^Ufs") %>% an_preferred(origin_country = c("United States of America", "Australia")))$country_name, rep("United States of America", 4))
})

test_that("an_filter behaves as expected", {
    ## filter by origin country
    g2 <- an_filter(g, origin_country = "Bulgaria")
    expect_true(all(g2$country_name == "Bulgaria"))

    ## filter by cga_source
    g2 <- an_filter(g, cga_source = "JPN")
    expect_true(all(g2$country_name == "Japan"))
    expect_true(all(g2$cga_source_gazetteer == "JPN"))
})
