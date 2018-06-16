context("fetching and caching data")

test_that("session caching works", {
    skip_on_cran()
    cdir <- file.path(tempdir(), "antanym-cache")
    cfile <- file.path(cdir, "gaz_data.csv")
    if (file.exists(cfile)) file.remove(cfile)
    g <- an_read(cache = "session")
    expect_true(file.exists(cfile))
    finfo <- file.info(cfile)

    ## re-read using cache
    g <- an_read(cache = "session")
    expect_identical(finfo$mtime, file.info(cfile)$mtime)

    ## refresh cache
    g <- an_read(cache = "session", refresh_cache = TRUE)

    ## mtime should have changed
    expect_gt(as.numeric(file.info(cfile)$mtime), as.numeric(finfo$mtime))
})

test_that("persistent caching works", {
    skip_on_cran()
    cdir <- rappdirs::user_cache_dir("antanym", "SCAR")
    cfile <- file.path(cdir, "gaz_data.csv")
    if (file.exists(cfile)) file.remove(cfile)
    g <- an_read(cache = "persistent")
    expect_true(file.exists(cfile))
    finfo <- file.info(cfile)

    ## re-read using cache
    g <- an_read(cache = "persistent")
    expect_identical(finfo$mtime, file.info(cfile)$mtime)

    ## refresh cache
    g <- an_read(cache = "persistent", refresh_cache = TRUE)

    ## mtime should have changed
    expect_gt(as.numeric(file.info(cfile)$mtime), as.numeric(finfo$mtime))
})

test_that("persistent caching to custom directory works", {
    skip_on_cran()
    cdir <- tempdir()
    cfile <- file.path(cdir, "gaz_data.csv")
    if (file.exists(cfile)) file.remove(cfile)
    g <- an_read(cache = cdir)
    expect_true(file.exists(cfile))
    finfo <- file.info(cfile)

    ## re-read using cache
    g <- an_read(cache = cdir)
    expect_identical(finfo$mtime, file.info(cfile)$mtime)

    ## refresh cache
    g <- an_read(cache = cdir, refresh_cache = TRUE)

    ## mtime should have changed
    expect_gt(as.numeric(file.info(cfile)$mtime), as.numeric(finfo$mtime))
})


