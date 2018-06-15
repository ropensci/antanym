## fetch the data once and cache it locally to use for testing
test_cache_dir <- tempfile(pattern = "antanym-cache")
if (!dir.exists(test_cache_dir)) dir.create(test_cache_dir)
an_read(cache_directory = test_cache_dir)
