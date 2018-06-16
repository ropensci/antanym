#' Load Antarctic place name data
#'
#' Place name data will be downloaded and optionally cached locally. If you wish to be able to use \code{antanym} offline, consider using \code{cache = "persistent"} so that the cached data will persist from one R session to the next. See \code{\link{an_cache_directory}} to get the path to the cache directory.
#'
#' @references \url{https://www.scar.org/data-products/place-names/}
#' @param gazetteers character: vector of gazetteers to load. For the list of available gazetteers, see \code{\link{an_gazetteers}}. Use \code{gazetteers = "all"} to load all available gazetteers. Currently only one gazetteer is available: the Composite Gazetteer of Antarctica
#' @param sp logical: if FALSE return a data.frame; if TRUE return a SpatialPointsDataFrame
#' @param cache string: the gazetteer data can be cached locally, so that it can be used offline later. Valid values are \code{"session"}, \code{"persistent"}, or a directory name. Specifying \code{cache = "session"} will use a temporary directory that persists only for the current session. \code{cache = "persistent"} will use \code{rappdirs::user_cache_dir()} to determine the appropriate directory to use. Otherwise, if a string is provided it will be assumed to be the path to the directory to use. In this case, an attempt will be made to create the cache directory if it does not exist. A warning will be given if a cached copy of the data exists and is more than 30 days old
#' @param refresh_cache logical: if TRUE, and a data file already exists in the cache, it will be refreshed. If FALSE, the cached copy will be used
#' @param simplified logical: if TRUE, only return a simplified set of columns (see details in "Value", below)
#' @param verbose logical: show progress messages?
#'
#' @return a data.frame or SpatialPointsDataFrame, with the following columns (note that not all information is populated for all place names):
#' \itemize{
#'   \item gaz_id - the unique identifier of each gazetteer entry. Note that the same feature (e.g. "Browns Glacier") might have multiple gazetteer entries, each with their own \code{gaz_id}, because the feature has been named multiple times by different naming authorities. The \code{scar_common_id} for these entries will be identical, because \code{scar_common_id} identifies the feature itself
#'   \item scar_common_id - the unique identifier (in the Composite Gazetteer of Antarctica) of the feature. A single feature may have multiple names, given by different naming authorities
#'   \item place_name - the name of the feature
#'   \item place_name_transliterated - the name of the feature transliterated to simple ASCII characters (e.g. with diacritical marks removed)
#'   \item longitude and latitude - the longitude and latitude of the feature (negative values indicate degrees west or south). Note that many features are not point features (e.g. mountains, lakes), in which case the \code{longitude} and \code{latitude} values are indicative only, generally of the centroid of the feature
#'   \item altitude - the altitude of the feature, in metres relative to sea level. Negative values indicate features below sea level
#'   \item feature_type_name - the feature type (e.g. "Archipelago", "Channel", "Mountain")
#'   \item date_named - the date on which the feature was named
#'   \item narrative - a text description of the feature; may include a synopsis of the history of its name
#'   \item named_for - the person after whom the feature was named, or other reason for its naming. For historical reasons the distinction between "narrative" and "named for" is not always obvious
#'   \item origin - the originating body of the name - either a country name, or organisation name for names that did not come from a national source
#'   \item relic - if \code{TRUE}, this name is associated with a feature that no longer exists (e.g. an ice shelf feature that has disappeared)
#'   \item gazetteer - the gazetteer from which this information came (currently only "CGA")
#' }
#' If \code{simplified} is FALSE, these additional columns will also be included:
#' \itemize{
#'   \item meeting_date - the date on which the name was formally approved by the associated national names committee. This is not available for many names: see the \code{date_named} column
#'   \item meeting_paper - references to papers or documents associated with the naming of the feature
#'   \item remote_sensor_info - text describing the remote sensing information (e.g. satellite platform name and image details) used to define the feature, if applicable
#'   \item coordinate_accuracy - an indicator of the accuracy of the coordinates, in metres
#'   \item altitude_accuracy - an indicator of the accuracy of the altitude value, in metres
#'   \item cga_source_gazetteer - for the Composite Gazetteer, this entry gives the source gazetteer from which this entry was taken. This is currently either a three-letter country code (e.g. "ESP", "USA") or "GEBCO" (for the GEBCO gazetteer of undersea features)
#'   \item country_name - the full name of the country where \code{cga_source_gazetteer} is a country
#'   \item source_name - the cartographic/GIS/remote sensing source from which the coordinates were derived
#'   \item source_publisher - where coordinates were derived from a map, the publisher of that map
#'   \item source_scale - the scale of the map from which the coordinates were derived
#'   \item source_institution - the institution from which the coordinate information came
#'   \item source_person - the contact person at the source institution, if applicable
#'   \item source_country_code - th country from which the coordinate information came
#'   \item source_identifier - where a coordinate or elevation was derived from a map, the unique identifier of that map
#'   \item comments - comments about the name or naming process
#' }
#' @examples
#' \dontrun{
#'  ## download without caching
#'  g <- an_read()
#'
#'  ## download and cache to a persistent directory for later, offline use
#'  g <- an_read(cache = "persistent")
#'
#'  ## refresh the cached copy
#'  g <- an_read(cache = "persistent", refresh_cache = TRUE)
#'
#'  ## download to session cache, in sp format
#'  g <- an_read(cache = "session", sp = TRUE)
#' }
#'
#' @seealso \code{\link{an_cache_directory}}, \code{\link{an_gazetteers}}
#'
#' @export
an_read <- function(gazetteers = "all", sp = FALSE, cache, refresh_cache = FALSE, simplified = TRUE, verbose = FALSE) {
    assert_that(!is.na(refresh_cache), is.flag(refresh_cache))
    assert_that(!is.na(verbose), is.flag(verbose))
    assert_that(!is.na(sp), is.flag(sp))
    assert_that(!is.na(simplified), is.flag(simplified))
    ## currently the gazetteers parameter does nothing, since we only have the CGA to load
    need_to_fetch_data <- FALSE
    local_file_name <- "gaz_data.csv"
    download_url <- "https://data.aad.gov.au/geoserver/aadc/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=aadc:SCAR_CGA_PLACE_NAMES&outputFormat=csv"
    if (!missing(cache) || nzchar(cache)) {
        cache_directory <- an_cache_directory(cache)
        ## create cache directory if necessary
        if (tolower(cache) == "session") {
            if (!dir.exists(cache_directory)) dir.create(cache_directory)
        } else if (tolower(cache) == "persistent") {
            if (!dir.exists(cache_directory)) dir.create(cache_directory, recursive = TRUE)
        } else {
            ## user has given a custom string, which we'll take to be the directory to use
            if (!dir.exists(cache_directory)) {
                if (verbose) message("creating data cache directory: ", cache_directory, "\n")
                ok <- dir.create(cache_directory)
                if (!ok) stop("could not create cache directory: ", cache_directory,
                              ". Consider creating the directory yourself and trying again.")
            }
        }
        ## so the cache directory will exist at this point
        ## it may or may not have a data file already present
        ## does data file exist?
        local_file_name <- file.path(cache_directory, local_file_name)
        if (refresh_cache || !file.exists(local_file_name)) need_to_fetch_data <- TRUE
        ## is cached copy old?
        if (file.exists(local_file_name)) {
            if (difftime(Sys.time(), file.info(local_file_name)$mtime, units = "days") > 30 && !refresh_cache)
                warning("cached copy of gazetteer data is more than 30 days old, ",
                        "consider refreshing your copy with an_read(..., refresh_cache = TRUE)")
        }
    } else {
        ## just provide the URL, and read_csv will fetch it
        local_file_name <- download_url
    }
    if (need_to_fetch_data) {
        if (verbose) message("downloading gazetteer data file to ", local_file_name, " ...")
        g <- do_fetch_data(download_url, verbose)
        ## cache it
        write.csv(g, file = local_file_name, fileEncoding = "UTF-8", row.names = FALSE, na = "")
    } else {
        if (verbose) {
            if (local_file_name == download_url)
                message("fetching gazetteer data from: ", local_file_name, "\n")
            else
                message("using cached copy of gazetteer data: ", local_file_name, "\n")
        }
    }
    if (local_file_name == download_url) {
        ## fetch using httr::GET, because read_csv chokes on SSL errors
        g <- do_fetch_data(download_url, verbose)
    } else {
        suppressMessages(g <- readr::read_csv(local_file_name))
    }
    ## rename "gazetteer" to "cga_source_gazetteer"
    names(g)[names(g) == "gazetteer"] <- "cga_source_gazetteer"
    ## add "gazetteer" column (meaning the overall gazetteer name, CGA in this case)
    g$gazetteer <- "CGA"

    ## place names are recorded in two formats: column "place_name_mapping" (e.g. Lake Thing) and "place_name_gazetteer" (Thing, Lake)
    ## for consistency with previous versions of the CGA, create a "place_name" column that we will use by default
    g$place_name <- g$place_name_mapping
    ## add version of place_name with no diacriticals
    g$place_name_transliterated <- stringi::stri_trans_general(g$place_name, "latin-ascii")

    g$relic <- grepl("yes", g$relic_flag, ignore.case = TRUE)

    `%eq%` <- function(x, y) !is.na(x) & !is.na(y) & x == y

    ## some ad-hoc fixes
    g$source_institution[tolower(g$source_institution) %eq% "aad"] <- "Australian Antarctic Division"
    g$source_institution[tolower(g$source_institution) %eq% "australian antarctic diviison"] <- "Australian Antarctic Division"
    g$source_institution[tolower(g$source_institution) %eq% "australian antarctic fdivision"] <- "Australian Antarctic Division"
    g$source_institution[tolower(g$source_institution) %eq% "australian antartic division"] <- "Australian Antarctic Division"
    g$source_institution[tolower(g$source_institution) %eq% "usgs"] <- "United States Geological Survey"

    g$cga_source_gazetteer[is.na(g$cga_source_gazetteer) & grepl("United States", g$country_name)] <- "USA"
    g$cga_source_gazetteer[is.na(g$cga_source_gazetteer) & grepl("Bulgaria", g$country_name)] <- "BGR"

    ## Amundsen Peak is somehow missing its lon/lat
    chk <- g$gaz_id == 137886 & is.na(g$longitude)
    if (sum(chk) == 1) g$longitude[chk] <- -61.7017
    chk <- g$gaz_id == 137886 & is.na(g$latitude)
    if (sum(chk) == 1) g$latitude[chk] <- -64.1294

    ## add "origin" column (country or organisation name)
    g$origin <- g$country_name
    g$origin[g$origin == "Unknown"] <- g$cga_source_gazetteer[g$origin == "Unknown"]

    g <- g[is.na(g$cga_source_gazetteer) | g$cga_source_gazetteer != "INFORMAL", ] ## informal names shouldn't be part of the CGA
    g <- g[, gaz_cols_to_show(g, simplified = simplified)]
    if (sp) {
        idx <- !is.na(g$longitude) & !is.na(g$latitude)
        g <- g[idx, ]
        coordinates(g) <- ~longitude+latitude
        projection(g) <- "+proj=longlat +datum=WGS84 +ellps=WGS84"
    }
    g
}

## internal helper function
do_fetch_data <- function(download_url, verbose) {
    temp <- GET(download_url, httr::config(ssl_verifypeer = 0L))
    if (http_error(temp)) {
        temp <- RETRY("GET", download_url, httr::config(ssl_verifypeer = 0L),
                      times = 3,
                      pause_base = 0.1,
                      pause_cap = 3,
                      quiet = !verbose)
    }
    if (http_error(temp)) stop("error downloading gazetteer data: ", http_status(temp)$message)
    suppressMessages(httr::content(temp, as = "parsed", encoding = "UTF-8"))
}



#' The cache directory used by antanym
#'
#' @param cache string: the gazetteer data can be cached locally, so that it can be used offline later. Valid values are \code{"session"}, \code{"persistent"}, or a directory name. Specifying \code{cache="session"} will use a temporary directory that persists only for the current session. \code{cache="persistent"} will use \code{rappdirs::user_cache_dir()} to determine the appropriate directory to use. Otherwise, the input string will be assumed to be the path to the directory to use
#'
#' @return directory path
#'
#' @seealso \code{\link{an_read}}
#'
#' @examples
#' ## per-session caching
#' an_cache_directory(cache = "session")
#'
#' ## persistent caching that will keep the data from one R session to the next
#' an_cache_directory(cache = "persistent")
#'
#' @export
an_cache_directory <- function(cache) {
    assert_that(is.string(cache), !is.na(cache), nzchar(cache))
    if (tolower(cache) == "session") {
        file.path(tempdir(), "antanym-cache")
    } else if (tolower(cache) == "persistent") {
        rappdirs::user_cache_dir("antanym", "SCAR")
    } else {
        ## user has given a custom string, which we'll take to be the directory to use
        cache
    }
}

## internal function, used to control the subset of columns returned to the user
gaz_cols_to_show <- function(gaz, simplified) {
    assert_that(inherits(gaz, c("data.frame", "SpatialPointsDataFrame")))
    nms <- c("gaz_id", "scar_common_id", "place_name", "place_name_transliterated", "longitude", "latitude", "altitude", "feature_type_name", "date_named", "narrative", "named_for", "origin", "relic", "gazetteer")
    if (!simplified) nms <- c(nms, c("meeting_date", "meeting_paper", "remote_sensor_info", "coordinate_accuracy", "altitude_accuracy", "cga_source_gazetteer", "country_name", "source_name", "source_scale", "source_publisher", "source_institution", "source_person", "source_country_code", "source_identifier", "comments"))
    intersect(nms, names(gaz))
}


