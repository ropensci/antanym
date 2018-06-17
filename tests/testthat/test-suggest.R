context("name suggestions")

g <- an_read(cache = "session")

test_that("an_suggest works as expected", {
    ext <- c(60, 90, -70, -60)
    dims <- c(100, 100)

    ## need either map_scale OR map_dimensions and map_extent
    expect_error(suggested <- an_suggest(g[1:100, ]))
    expect_error(suggested <- an_suggest(g[1:100, ], map_extent = ext))
    expect_error(suggested <- an_suggest(g[1:100, ], map_dimensions = dims))

    ## ok
    suggested <- an_suggest(g[1:100, ], map_scale = 12e6)
    suggested <- an_suggest(g[1:100, ], map_extent = ext, map_dimensions = dims)

    ## too small a scale
    expect_error(suggested <- an_suggest(g[1:100, ], map_scale = 1e6), "not yet implemented for map_scale")

    ## map extent and scale
    ms <- an_mapscale(map_dimensions = dims, map_extent = ext)
    suggested <- an_suggest(g[1:100, ], map_scale = ms, map_extent = ext)
    ## should give same result as extent and dims
    suggested2 <- an_suggest(g[1:100, ], map_extent = ext, map_dimensions = dims)
    expect_identical(suggested, suggested2)

    ## extent can be passed in various formats
    ## as a raster object
    raster_ext <- raster()
    extent(raster_ext) <- ext
    suggested2 <- an_suggest(g[1:100, ], map_scale = ms, map_extent = raster_ext)
    expect_identical(suggested, suggested2)
    ## as a raster extent object
    suggested2 <- an_suggest(g[1:100, ], map_scale = ms, map_extent = extent(raster_ext))
    expect_identical(suggested, suggested2)
    ## Spatial object
    my_sp <- sp::SpatialPoints(cbind(c(60, 90), c(-70, -60)))
    suggested2 <- an_suggest(g[1:100, ], map_scale = ms, map_extent = my_sp)
    expect_identical(suggested, suggested2)
    ## Spatial bbox object
    suggested2 <- an_suggest(g[1:100, ], map_scale = ms, map_extent = sp::bbox(my_sp))
    expect_identical(suggested, suggested2)
})
