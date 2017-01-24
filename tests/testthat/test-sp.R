context("antanym")

test_that("sp option returns an sp object", {
    gsp <- an_read(sp=TRUE)
    expect_s4_class(gsp,"SpatialPointsDataFrame")
})

test_that("sp versions of functions match non-sp for lon-lat data", {
    ext <- c(100,110,-70,-60)
    expect_gt(nrow(an_filter(g,extent=ext)),100)
    expect_equal(nrow(an_filter(g,extent=ext)),nrow(an_filter(gsp,extent=ext)))
    expect_identical(an_countries(g),an_countries(gsp))
    expect_identical(an_feature_types(g),an_feature_types(gsp))
    expect_identical(an_cga_sources(g),an_cga_sources(gsp))

    pt <- c(110,-66)
    namelist <- function(z)sort(z$place_name)
    expect_identical(namelist(an_near(g,pt,50)),namelist(an_near(gsp,pt,50)))
    pt <- SpatialPoints(cbind(110,-66))
    projection(pt) <- "+proj=longlat +datum=WGS84 +ellps=WGS84"
    expect_identical(namelist(an_near(g,pt,50)),namelist(an_near(gsp,pt,50)))
})

test_that("sp versions of functions match non-sp for projected data", {
    stersouth <-  "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
    gsp2 <- spTransform(gsp,CRS(stersouth))
    expect_identical(an_countries(g),an_countries(gsp2))
    expect_identical(an_feature_types(g),an_feature_types(gsp2))
    expect_identical(an_cga_sources(g),an_cga_sources(gsp2))

    pt2 <- SpatialPoints(cbind(110,-66))
    projection(pt2) <- "+proj=longlat +datum=WGS84 +ellps=WGS84"
    pt2 <- spTransform(pt2,stersouth)
    expect_identical(namelist(an_near(g,pt,20)),namelist(an_near(gsp2,pt2,20)))
    ## namelists won't be the same for larger distances, because great-circle distances
    ## calculated from lon-lat data won't be the same as distances calculated from
    ## projected polar-stereo data
    expect_false(identical(namelist(an_near(g,pt,50)),namelist(an_near(gsp2,pt2,50))))
})
