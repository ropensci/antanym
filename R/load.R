#' Load Antarctic and subantarctic place name gazetteers
#'
#' @references \url{http://www.scar.org/data-products/cga} \url{http://data.aad.gov.au/aadc/gaz/}
#' @param gazetteers character: vector of gazetteers to load. For the list of available gazetteers, see \code{\link{an_gazetteers}}. Use \code{gazetteers="all"} to load all available gazetteers
#' @param sp logical: if FALSE return a data.frame; if TRUE return a SpatialPointsDataFrame
#' @param cache_directory string: (optional) cache the gazetteer data file locally in this directory, so that it can be used offline later. The cache directory will be created if it does not exist. A warning will be given if a cached copy exists and is more than 30 days old
#' @param refresh_cache logical: if TRUE, and a data file already exists in the cache_directory, it will be refreshed. If FALSE, the cached copy will be used
#' @param verbose logical: show progress messages?
#'
#' @return a data.frame or SpatialPointsDataFrame
#'
#' @examples
#' \dontrun{
#'  g <- an_read(cache_directory="c:/temp/gaz")
#' }
#'
#' @export
an_read <- function(gazetteers = "all", sp = FALSE, cache_directory, refresh_cache = FALSE, verbose = FALSE) {
    assert_that(is.flag(refresh_cache))
    assert_that(is.flag(verbose))
    assert_that(is.flag(sp))
    ## currently the gazetteers parameter does nothing, since we only have the CGA to load
    do_cache_locally <- FALSE
    local_file_name <- "gaz_data.csv"
    ##download_url <- "https://data.aad.gov.au/geoserver/aadc/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=aadc:SCAR_CGA_PLACE_NAMES_SIMPLIFIED&outputFormat=csv"
    ## or for the complete set of columns
    download_url <- "https://data.aad.gov.au/geoserver/aadc/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=aadc:SCAR_CGA_PLACE_NAMES&outputFormat=csv"
    if (!missing(cache_directory)) {
        assert_that(is.string(cache_directory))
        if (!dir.exists(cache_directory)) {
            if (verbose) message("creating data cache directory: ", cache_directory, "\n")
            ok <- dir.create(cache_directory)
            if (!ok) stop("could not create cache directory: ", cache_directory)
            do_cache_locally <- TRUE
        } else {
            ## cache dir exists
            ## does data file exist
            local_file_name <- file.path(cache_directory, local_file_name)
            if (refresh_cache || !file.exists(local_file_name)) do_cache_locally <- TRUE
            ## is cached copy old?
            if (file.exists(local_file_name)) {
                if (difftime(Sys.time(), file.info(local_file_name)$mtime, units="days")>30 && !refresh_cache)
                    warning("cached copy of gazetteer data is more than 30 days old, consider refreshing your copy with an_read(..., refresh_cache=TRUE)")
            }
        }
    } else {
        ## just provide the URL, and read_csv will fetch it
        local_file_name <- download_url
    }
    if (do_cache_locally) {
        if (verbose) message("downloading gazetter data file to ", local_file_name, " ...")
        g <- do_fetch_data(download_url)
        ## cache it
        write.csv(g, file = local_file_name, fileEncoding = "UTF-8", row.names = FALSE, na = "")
    } else {
        if (verbose) {
            if (local_file_name==download_url)
                message("fetching gazetteer data from: ", local_file_name, "\n")
            else
                message("using cached copy of gazetteer data: ", local_file_name, "\n")
        }
    }
    if (local_file_name==download_url) {
        ## fetch using httr::GET, because read_csv chokes on SSL errors
        g <- do_fetch_data(download_url)
    } else {
        suppressMessages(g <- readr::read_csv(local_file_name))
    }
    ## rename "gazetteer" to "cga_source_gazetteer"
    names(g)[names(g)=="gazetteer"] <- "cga_source_gazetteer"
    ## add "gazetteer" column (meaning the overall gazetteer name, cga in this case)
    g$gazetteer <- "cga"
    ## previously we had just "place_name" column, now "place_name_mapping" (e.g. Lake Thing) and "place_name_gazetteer" (Thing, Lake)
    g$place_name <- g$place_name_mapping
    ## add version of place_name with no diacriticals
    g$place_name_transliterated <- stringi::stri_trans_general(g$place_name, "latin-ascii")

    ## some ad-hoc fixes
    g <- g[is.na(g$cga_source_gazetteer) | g$cga_source_gazetteer!="INFORMAL",] ## informal names shouldn't be part of the CGA
    g <- g[,gaz_names_to_show(g)]
    if (sp) {
        idx <- !is.na(g$longitude) & !is.na(g$latitude)
        g <- g[idx,]
        coordinates(g) <- ~longitude+latitude
        projection(g) <- "+proj=longlat +datum=WGS84 +ellps=WGS84"
    }
    g
}

do_fetch_data <- function(download_url) {
    temp <- GET(download_url, httr::config(ssl_verifypeer = 0L))
    if (http_error(temp)) stop("error downloading gazetteer data: ",http_status(temp)$message)
    suppressMessages(httr::content(temp, as = "parsed", encoding = "UTF-8"))
}

#' @rdname an_read
#' @export
an_gazetteers <- function() c("cga") ## for now, the CGA is the only gazetteer provided


## internal function, used to control the subset of columns returned to the user
gaz_names_to_show <- function(gaz) intersect(names(gaz), c("gaz_id", "place_name", "latitude", "longitude", "altitude", "feature_type_name", "narrative", "named_for", "meeting_date", "meeting_paper", "date_revised", "cga_source_gazetteer", "scar_common_id", "is_complete_flag", "remote_sensor_info", "coordinate_accuracy", "altitude_accuracy", "source_institution", "source_person", "source_country_code", "source_name", "comments", "source_publisher", "source_identifier", "date_named", "country_name", "gazetteer", "place_name_transliterated"))

