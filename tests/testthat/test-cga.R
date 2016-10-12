context("agn")

test_that("subsetting works", {
    expect_equal(nrow(agn_near(g,c(100,-66),20)),5)
    expect_gt(nrow(g %>% agn_near(c(100,-66),20)),nrow(g %>% agn_near(c(100,-66),20) %>% agn_filter(feature_type="Island")))
})
