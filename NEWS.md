# antanym 0.4.0

General revisions following rOpenSci review. Note several breaking changes:

  - `an_read` now takes a `cache` parameter instead of `cache_directory` (and now can have special values "session" and "persistent")
  - `an_filter` and `an_suggest` now take an `origin` parameter that replaces the previous `origin_country` and `cga_source` parameters
  - the default data structure (returned by `an_read(..., simplified = TRUE)` no longer contains the "country_name" or "cga_source_gazetteer columns, but if needed these are available via `an_read(..., simplified = FALSE)`
  

# antanym 0.3.0

* Added a `NEWS.md` file to track changes to the package.

