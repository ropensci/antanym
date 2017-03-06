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
    gxy <- spTransform(gsp,CRS(stersouth))
    expect_identical(an_countries(g),an_countries(gxy))
    expect_identical(an_feature_types(g),an_feature_types(gxy))
    expect_identical(an_cga_sources(g),an_cga_sources(gxy))

    pt <- testpt
    pt2 <- SpatialPoints(cbind(testpt[1],testpt[2]))
    projection(pt2) <- projll
    pt2 <- spTransform(pt2,stersouth)
    expect_identical(namelist(an_near(g,pt,20)),namelist(an_near(gxy,pt2,20)))
    ## previously, namelists would not be the same for larger distances, because we were using great-circle distances
    ## for lon-lat data and x-y coords for projected data
    ## but now all are great-circles, so these should now match
    expect_identical(namelist(an_near(g,pt,50)),namelist(an_near(gxy,pt2,50)))

    expect_identical(namelist(an_preferred(g,"Australia")),
                     namelist(an_preferred(gxy,"Australia")))
    expect_identical(namelist(an_preferred(g,c("Australia","Poland"))),
                     namelist(an_preferred(gxy,c("Australia","Poland"))))

    expect_identical(an_url(g[1:10,]),an_url(gxy[1:10,]))

    ## these tests don't work, because projected and lonlat extents will never cover the same actual regions because one is rectangular in lonlat and the other is rectangular in projected space
    ##
    ##ext <- c(60,90,-70,-60)
    ##ext2 <- raster(extent(ext))
    ##projection(ext2) <- projll
    ##ext2 <- projectExtent(ext2,CRS(stersouth))
    ##suggestedsp <- an_suggest(gxy,map_extent=extent(ext2),map_dimensions=c(100,100))
    #### now ext2 doesn't cover the same coverage in polar stereo as ext does in lonlat, so now
    #### reverse project ext2 to get the same region
    ##suggested <- an_suggest(g,map_extent=extent(projectExtent(ext2,CRS(projll))),map_dimensions=c(100,100))
    ##expect_identical(namelist(suggested),namelist(suggestedsp))
    ##expect_identical(namelist(an_thin(suggested,10)),namelist(an_thin(suggestedsp,10)))
})

test_that("an_near works with projected/unprojected data", {
    skip_if_not_installed("rgdal")

    ## an_near should give the same answers regardless of whether the gaz object is a standard data.frame or an sp object, or if it is projected, and same for the target location

    projll <- "+proj=longlat +datum=WGS84 +ellps=WGS84"
    stersouth <-  "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
    gxy <- spTransform(gsp,CRS(stersouth))

    loc <- c(110,-66)
    locsp <- SpatialPoints(matrix(loc,nrow=1),proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))
    locxy <- spTransform(locsp,CRS(stersouth))

    n11 <- an_near(g,loc,10)
    n12 <- an_near(g,locsp,10)
    n13 <- an_near(g,locxy,10)
    expect_identical(n11,n12)
    expect_identical(n11,n13)

    n21 <- an_near(gsp,loc,10)
    n22 <- an_near(gsp,locsp,10)
    n23 <- an_near(gsp,locxy,10)
    expect_identical(n21,n22)
    expect_identical(n21,n23)

    n31 <- an_near(gxy,loc,10)
    n32 <- an_near(gxy,locsp,10)
    n33 <- an_near(gxy,locxy,10)
    expect_identical(n31,n32)
    expect_identical(n31,n33)

    expect_identical(unname(as.matrix(n11[,c("longitude","latitude")])),unname(coordinates(n21)))
})
