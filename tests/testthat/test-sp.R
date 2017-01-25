context("antanym spatial objects")

gsp <- an_read(sp=TRUE)
testpt <- c(110,-66)
namelist <- function(z)sort(z$place_name)

test_that("sp option returns an sp object", {
    expect_s4_class(gsp,"SpatialPointsDataFrame")
})

test_that("sp versions of functions match non-sp for lon-lat data", {
    ext <- c(100,110,-70,-60)
    expect_gt(nrow(an_filter(g,extent=ext)),100)
    expect_identical(namelist(an_filter(g,extent=ext)),namelist(an_filter(gsp,extent=ext)))
    expect_identical(an_countries(g),an_countries(gsp))
    expect_identical(an_feature_types(g),an_feature_types(gsp))
    expect_identical(an_cga_sources(g),an_cga_sources(gsp))

    pt <- testpt
    expect_identical(namelist(an_near(g,pt,50)),namelist(an_near(gsp,pt,50)))
    pt <- SpatialPoints(cbind(testpt[1],testpt[2]))
    projection(pt) <- "+proj=longlat +datum=WGS84 +ellps=WGS84"
    expect_identical(namelist(an_near(g,pt,50)),namelist(an_near(gsp,pt,50)))

    expect_identical(namelist(an_preferred(g,"Australia")),
                     namelist(an_preferred(gsp,"Australia")))
    expect_identical(namelist(an_preferred(g,c("Australia","Poland"))),
                     namelist(an_preferred(gsp,c("Australia","Poland"))))

    expect_identical(an_url(g[1:10,]),an_url(gsp[1:10,]))

    ext <- c(60,90,-70,-60)
    suggested <- an_suggest(g,map_extent=ext,map_dimensions=c(100,100))
    suggestedsp <- an_suggest(gsp,map_extent=ext,map_dimensions=c(100,100))

    expect_identical(namelist(suggested),namelist(suggestedsp))
    expect_identical(namelist(an_thin(suggested,10)),namelist(an_thin(suggestedsp,10)))

})

test_that("sp versions of functions match non-sp for projected data", {
    ## spTransform requires rgdal, but the rest of antanym doesn't, so it's only a suggested packagex
    skip_if_not_installed("rgdal")

    projll <- "+proj=longlat +datum=WGS84 +ellps=WGS84"
    stersouth <-  "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
    gsp2 <- spTransform(gsp,CRS(stersouth))
    expect_identical(an_countries(g),an_countries(gsp2))
    expect_identical(an_feature_types(g),an_feature_types(gsp2))
    expect_identical(an_cga_sources(g),an_cga_sources(gsp2))

    pt <- testpt
    pt2 <- SpatialPoints(cbind(testpt[1],testpt[2]))
    projection(pt2) <- projll
    pt2 <- spTransform(pt2,stersouth)
    expect_identical(namelist(an_near(g,pt,20)),namelist(an_near(gsp2,pt2,20)))
    ## namelists won't be the same for larger distances, because great-circle distances
    ## calculated from lon-lat data won't be the same as distances calculated from
    ## projected polar-stereo data
    expect_false(identical(namelist(an_near(g,pt,50)),namelist(an_near(gsp2,pt2,50))))

    expect_identical(namelist(an_preferred(g,"Australia")),
                     namelist(an_preferred(gsp2,"Australia")))
    expect_identical(namelist(an_preferred(g,c("Australia","Poland"))),
                     namelist(an_preferred(gsp2,c("Australia","Poland"))))

    expect_identical(an_url(g[1:10,]),an_url(gsp2[1:10,]))

    ## these tests don't work, because projected and lonlat extents will never cover the same actual regions because one is rectangular in lonlat and the other is rectangular in projected space
    ##
    ##ext <- c(60,90,-70,-60)
    ##ext2 <- raster(extent(ext))
    ##projection(ext2) <- projll
    ##ext2 <- projectExtent(ext2,CRS(stersouth))
    ##suggestedsp <- an_suggest(gsp2,map_extent=extent(ext2),map_dimensions=c(100,100))
    #### now ext2 doesn't cover the same coverage in polar stereo as ext does in lonlat, so now
    #### reverse project ext2 to get the same region
    ##suggested <- an_suggest(g,map_extent=extent(projectExtent(ext2,CRS(projll))),map_dimensions=c(100,100))
    ##expect_identical(namelist(suggested),namelist(suggestedsp))
    ##expect_identical(namelist(an_thin(suggested,10)),namelist(an_thin(suggestedsp,10)))
})
