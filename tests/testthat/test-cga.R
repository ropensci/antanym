context("CGA")

test_that("subsetting works", {
    g <- load_cga()
    expect_equal(nrow(names_near(g,c(100,-66),20)),5)
    expect_gt(nrow(g %>% names_near(c(100,-66),20)),nrow(g %>% names_near(c(100,-66),20) %>% subset_names(feature_type="Island")))
})
