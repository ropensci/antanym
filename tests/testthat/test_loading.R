context("fetching and caching data")

test_that("caching works", {
    skip_on_cran()
    cdir <- tempdir()
    cfile <- file.path(cdir,"gaz_data.csv")
    if (file.exists(cfile)) file.remove(cfile)
    g <- an_read(cache_dir=cdir)
    expect_true(file.exists(cfile))
    finfo <- file.info(cfile)
    ## re-read using cache
    g <- an_read(cache_dir=cdir)
    expect_identical(finfo,file.info(cfile))
    ## refresh cache
    g <- an_read(cache_dir=cdir,refresh_cache=TRUE)
    expect_false(identical(finfo,file.info(cfile)))
    ## mtime should have changed
    expect_gt(as.numeric(file.info(cfile)$mtime),as.numeric(finfo$mtime))
})


