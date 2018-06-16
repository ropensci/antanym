context("filtering of names data")

g <- an_read(cache = "session")
gsp <- an_read(cache = "session", sp = TRUE)

test_that("an_filter behaves as expected", {
    expect_identical(g, an_filter(g)) ## no filtering applied
    ## illegal filters (NA or empty string, or not-a-string)
    expect_error(an_filter(g, query = NA_character_))
    expect_error(an_filter(g, query = ""))
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

    ## filter by gazetteer (which is only "CGA" at the moment)
    expect_identical(g,an_filter(g, origin_gazetteer = "CGA"))
    expect_equal(nrow(an_filter(g, origin_gazetteer = "notagazetteer")),0)
})

test_that("the various extent options work properly in an_filter", {
    ext <- c(100, 110, -70, -60)
    my_sp <- SpatialPoints(cbind(c(100, 110), c(-70, -60)))
    my_raster <- raster(extent(ext))

    expect_identical(an_filter(g, extent = bbox(my_sp)), an_filter(g, extent = ext))
    expect_identical(an_filter(g, extent = my_sp), an_filter(g, extent = ext))
    expect_identical(an_filter(g, extent = my_raster), an_filter(g, extent = ext))
    expect_identical(an_filter(g, extent = extent(my_raster)), an_filter(g, extent = ext))

    expect_identical(an_filter(gsp, extent = bbox(my_sp)), an_filter(gsp, extent = ext))
    expect_identical(an_filter(gsp, extent = my_sp), an_filter(gsp, extent = ext))
    expect_identical(an_filter(gsp, extent = extent(my_raster)), an_filter(gsp, extent = ext))
    expect_identical(an_filter(gsp, extent = my_raster), an_filter(gsp, extent = ext))

    ## note that testing of filtering of sp vs non-sp inputs is done in test-sp.R
})

test_that("text filtering works properly in an_filter", {
    ## defaults are regex and case-insensitive
    expect_gt(nrow(an_filter(g, "William Scoresby Archipelago")), 0)
    expect_gt(nrow(an_filter(g, "william scoresby archipelago")), 0)
    expect_gt(nrow(an_filter(g, "william scoresby archipelago", as_regex=FALSE)), 0)
    expect_equal(nrow(an_filter(g, "william scoresby archipelago", ignore_case=FALSE)), 0)

    ## no matches to this because the actual name is 'William Scoresby Archipelago'
    expect_equal(nrow(an_filter(g, "william archipelago", as_regex=FALSE)), 0)
    expect_equal(nrow(an_filter(g, "william archipelago")), 0)

    ## providing a regex
    expect_gt(nrow(an_filter(g, "william .* archipelago")), 0)
    expect_gt(nrow(an_filter(g, "William .* Archipelago")), 0)
    expect_equal(nrow(an_filter(g, "william .* archipelago", ignore_case=FALSE)), 0)

    ## multiple search terms
    expect_gt(nrow(an_filter(g, c("william", "archipelago"))), 0)
    expect_equal(nrow(an_filter(g, c("william", "archipelago"), ignore_case=FALSE)), 0)

    ## with feature type
    expect_gt(nrow(an_filter(g, "William", feature_type = "Archipelago")), 0)
    expect_gt(nrow(an_filter(g, "william", feature_type = "Archipelago")), 0)
    expect_equal(nrow(an_filter(g, "william", feature_type = "Archipelago", ignore_case=FALSE)), 0)
    expect_equal(nrow(an_filter(g, "William", feature_type = "archipelago", ignore_case=FALSE)), 0)
})

test_that("searching by spatial proximity works", {
    expect_equal(nrow(an_near(g, c(100, -66), 20)), 5)
    expect_gt(nrow(g %>% an_near(c(100, -66), 20)), nrow(g %>% an_near(c(100, -66), 20) %>% an_filter(feature_type = "Island")))
})
