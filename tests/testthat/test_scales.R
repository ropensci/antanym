context("antanym utilities")

test_that("map scale estimation works", {
    ## 1:20M map
    expect_equal(round(an_mapscale(c(400, 570), c(-180, 180, -90, -40)) /1e6), 20)

    ## 1:1M map
    expect_equal(round(an_mapscale(c(550, 550), c(68, 81, -70, -65))/1e6), 1)

    ## 1:50k map
    expect_lte(abs(round(an_mapscale(c(650, 750), c(158+36/60, 159+6/60, -54-47.5/60, -54-28/60))/1e3)-50), 1)

    ## 50M map
    expect_lte(abs(round(an_mapscale(c(150, 150), c(-180, 180, -90, -50))/1e6)-50), 1)
})
