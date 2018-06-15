context("antanym")

g <- an_read(cache = "session")

test_that("subsetting works", {
    expect_equal(nrow(an_near(g, c(100, -66), 20)), 5)
    expect_gt(nrow(g %>% an_near(c(100, -66), 20)), nrow(g %>% an_near(c(100, -66), 20) %>% an_filter(feature_type = "Island")))
})

test_that("preferred name selection works", {
    expect_equal((g %>% an_filter("^Ufs") %>% an_preferred(origin_country = c("Australia", "United States of America")))$country_name, c("Australia", "United States of America", "United States of America", "United States of America"))
    expect_equal((g %>% an_filter("^Ufs") %>% an_preferred(origin_country = c("United States of America", "Australia")))$country_name, rep("United States of America", 4))
})

test_that("an_filter behaves as expected", {
    expect_identical(g, an_filter(g)) ## no filtering applied
    ## illegal filters (NA or empty string, or not-a-string)
    expect_error(an_filter(g, query = NA_character_))
    expect_error(an_filter(g, query = ""))
    expect_error(an_filter(g, query = c("1","2")))
    expect_error(an_filter(g, query = 6))

    ## filter by name, name_transliterated
    g2 <- an_filter(g,query = "^Som")
    expect_true(all(grepl("^Som",g2$place_name_transliterated)))

    ## filter by region of interest
    g2 <- an_filter(g, extent = c(140, 150, -66, -65))
    expect_true(all(g2$longitude >= 140))
    expect_true(all(g2$longitude <= 150))
    expect_true(all(g2$latitude >= -66))
    expect_true(all(g2$latitude <= -65))

    ## filter by feature type
    g2 <- an_filter(g, feature_type = "Pyramid")
    expect_true(all(g2$feature_type_name == "Pyramid"))

    ## filter by origin country
    g2 <- an_filter(g, origin_country = "Bulgaria")
    expect_true(all(g2$country_name == "Bulgaria"))

    ## filter by gazetteer (which is only "CGA" at the moment)
    expect_identical(g,an_filter(g, origin_gazetteer = "CGA"))
    expect_equal(nrow(an_filter(g, origin_gazetteer = "notagazetteer")),0)

    ## filter by cga_source
    g2 <- an_filter(g, cga_source = "JPN")
    expect_true(all(g2$country_name == "Japan"))
    expect_true(all(g2$cga_source_gazetteer == "JPN"))
})
