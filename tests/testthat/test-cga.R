context("agn")

test_that("subsetting works", {
    expect_equal(nrow(agn_near(g,c(100,-66),20)),5)
    expect_gt(nrow(g %>% agn_near(c(100,-66),20)),nrow(g %>% agn_near(c(100,-66),20) %>% agn_filter(feature_type="Island")))
})

test_that("preferred name selection works", {
    expect_equal((g %>% agn_filter("^Ufs") %>% agn_preferred(origin_country=c("Australia","United States of America")))$country_name,c("Australia","United States of America","United States of America","United States of America"))
    expect_equal((g %>% agn_filter("^Ufs") %>% agn_preferred(origin_country=c("United States of America","Australia")))$country_name,rep("United States of America",4))
})
