context("name thinning")

g <- an_read(cache = "session")

test_that("thinning works as expected", {
    ext <- c(60, 90, -70, -60)
    suggested <- an_suggest(g, map_extent = ext, map_dimensions = c(100, 100))

    ## by default should get an error if we pass the whole g data frame
    ## because nrow(g) > row_limit
    expect_error(an_thin(g, 10))

    ## similarly if nrow of input is greater than row_limit
    temp <- suggested[1:50, ]
    expect_error(an_thin(temp, 10, row_limit=5))
    ## but ok if we up the limit
    blah <- an_thin(temp, 10, row_limit=100)

    ## can specify different score col
    t1 <- an_thin(suggested, 10)
    names(suggested)[names(suggested)=="score"] <- "score2"
    t2 <- an_thin(suggested, 10, score_col = "score2")
    names(t2)[names(t2)=="score2"] <- "score"
    expect_identical(t1, t2)

    names(suggested)[names(suggested)=="score2"] <- "score"

    ## can't thin names if we have less scores than requested number of names
    suggested$score <- NA_real_
    suggested$score[1:10] <- 1
    expect_error(an_thin(suggested, 20))
    ## but it'll work if we only need 10 names
    t1 <- an_thin(suggested, 10)

    ## works without score column
    suggested$score <- 10
    t1 <- an_thin(suggested, 10)
    t2 <- an_thin(suggested, 10, score_col = "hahaha")
    expect_identical(t1, t2)

    ## uniform or missing scores should give same as score_weighting of 0
    t2 <- an_thin(suggested, 10, score_weighting = 0)
    expect_identical(t1, t2)

    ## scaling of scores should make no difference
    suggested <- an_suggest(g, map_extent = ext, map_dimensions = c(100, 100))
    t1 <- an_thin(suggested, 10)
    t1$score <- 0 ## just so we don't get differences because of score
    suggested$score <- suggested$score*10
    t2 <- an_thin(suggested, 10)
    t2$score <- 0 ## just so we don't get differences because of score
    expect_identical(t1, t2)
})
