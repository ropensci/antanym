#' Load the Composite Gazetteer of Antarctica
#'
#' @references \url{http://www.scar.org/data-products/cga}
#' @param cache_directory string: (optional) cache the CGA data file locally in this directory, so that it can be used offline later. The cache directory will be created if it does not exist
#' @param refresh_cache logical: if TRUE, and a data file already exists in the cache_directory, it will be refreshed. If FALSE, the cached copy will be used
#' @param verbose logical: show progress messages?
#'
#' @return An object of class \code{SQLiteDriver}
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
    download_url <- "http://data.aad.gov.au/geoserver/aadc/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=aadc:SCAR_CGA_PLACE_NAMES_NEW_SIMPLIFIED&outputFormat=csv"
    ## or for the complete set of columns "http://data.aad.gov.au/geoserver/aadc/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=aadc:SCAR_CGA_PLACE_NAMES_NEW&outputFormat=csv"
    if (!missing(cache_directory)) {
        assert_that(is.string(cache_directory))
        if (!dir.exists(cache_directory)) {
            if (verbose) cat("creating cga cache directory: ",cache_directory,"\n")
            ok <- dir.create(cache_directory)
            if (!ok) stop("could not create cache directory: ",cache_directory)
            do_download <- TRUE
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
        download.file(download_url,destfile=local_file_name,quiet=!verbose)
        if (verbose) cat("done.\n")
    } else {
        if (verbose) cat("using cached copy of cga: ",local_file_name,"\n")
    }
    g <- readr::read_csv(local_file_name)

    ## split display scales into separate columns
    #temp <- lapply(g$display_scales,strsplit,split=",")
    #all_scales <- na.omit(as.numeric(unique(sapply(temp,function(z)z[[1]][[1]]))))

    con <- dbConnect(RSQLite::SQLite(),tempfile(fileext=".sqlite"))
    dbWriteTable(con,"cga",g)
    con
}

