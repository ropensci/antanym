context("filtering of names data")

g <- an_read(cache = "session")
gsp <- an_read(cache = "session", sp = TRUE)

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
