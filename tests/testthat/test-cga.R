context("CGA-specific functionality")

g <- an_read(cache = "session")

test_that("preferred name selection works", {
    expect_warning(expect_equal((g %>% an_filter("^Ufs") %>% an_preferred(origin_country = c("Australia", "United States of America")))$country_name, c("Australia", "United States of America", "United States of America", "United States of America")), "deprecated")
    expect_warning(expect_equal((g %>% an_filter("^Ufs") %>% an_preferred(origin_country = c("United States of America", "Australia")))$country_name, rep("United States of America", 4)), "deprecated")

    expect_equal((g %>% an_filter("^Ufs") %>% an_preferred(cga_source = c("AUS", "USA")))$country_name, c("Australia", "United States of America", "United States of America", "United States of America"))
    expect_equal((g %>% an_filter("^Ufs") %>% an_preferred(cga_source = c("USA", "AUS")))$country_name, rep("United States of America", 4))

    ## compare results from country and source_gaz methods
    g1 <- an_preferred(g, cga_source="POL")
    expect_warning(g2 <- an_preferred(g, origin_country = "Poland"), "deprecated")
    ## for the features named preferred country, these should be the same
    g1 <- g1[g1$country_name=="Poland", ]
    g2 <- g2[g2$country_name=="Poland", ]
    expect_identical(g1, g2)
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

test_that("cga data integrity", {
    expect_false(any(is.na(g$cga_source_gazetteer)))
    expect_false(any(duplicated(g$gaz_id)))
    expect_false(any(is.na(g$gaz_id)))
    expect_false(any(is.na(g$scar_common_id)))
    expect_false(any(is.na(g$longitude)))
    expect_false(any(is.na(g$latitude)))
})
