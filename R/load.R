#' Load the Composite Gazetteer of Antarctica
#'
#' @references \url{http://www.scar.org/data-products/cga}
#' @param cache_directory string: (optional) cache the CGA data file locally in this directory, so that it can be used offline later. The cache directory will be created if it does not exist
#' @param refresh_cache logical: if TRUE, and a data file already exists in the cache_directory, it will be refreshed. If FALSE, the cached copy will be used
#' @param verbose logical: show progress messages?
#'
#' @return data.table
#'
#' @examples
#' \dontrun{
#'  g <- load_cga(cache_directory="c:/temp/cga")
#' }
#'
#' @export
load_cga <- function(cache_directory,refresh_cache=FALSE,verbose=TRUE) {
    assert_that(is.flag(refresh_cache))
    assert_that(is.flag(verbose))
    do_download <- FALSE
    local_file_name <- "cga_data.csv"
    if (!missing(cache_directory)) {
        assert_that(is.string(cache_directory))
        if (!dir.exists(cache_directory)) {
            if (verbose) cat("creating cga cache directory: ",cache_directory,"\n")
            ok <- dir.create(cache_directory)
            if (!ok) stop("could not create cache directory: ",cache_directory)
        } else {
            ## cache dir exists
            ## does data file exist
            local_file_name <- file.path(cache_directory,local_file_name)
            if (refresh_cache || !file.exists(local_file_name)) do_download <- TRUE
        }
    } else {
        local_file_name <- file.path(tempdir(),local_file_name)
        do_download <- TRUE
    }
    if (do_download) {
        if (verbose) cat("downloading cga data file to ",local_file_name," ...")
        download.file("http://data.aad.gov.au/geoserver/aadc/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=aadc:SCAR_CGA_PLACE_NAMES_NEW&outputFormat=csv",destfile=local_file_name,quiet=!verbose)
        if (verbose) cat("done.\n")
    } else {
        if (verbose) cat("using cached copy of cga: ",local_file_name,"\n")
    }
    readr::read_csv(local_file_name)
}

