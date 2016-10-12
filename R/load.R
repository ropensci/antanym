#' Load Antarctic and subantarctic place name gazetteers
#'
#' The SCAR Composite Gazetteer of Antarctica (CGA), as the name suggests, is a composite or collection of all those names of features that have been submitted by representatives of national gazetteers. It includes the names of features south of 60° S, both terrestrial and undersea or under-ice. The CGA is a compilation of recognized features, with a numerical Unique Identifier code (UID) assigned to each of them, jointly with a list of applicable place names.
#' Since 2008, Italy and Australia jointly have managed the CGA, the former taking care of the editing, the latter maintaining database and website. The SCAR Standing Committee on Antarctic Geographic Information (SCAGI) coordinates the project.
#'
#' @references \url{http://www.scar.org/data-products/cga} \url{http://data.aad.gov.au/aadc/gaz/}
#' @param gazetteers character: vector of gazetteers to load. For the list of available gazetteers, see \code{\link{an_gazetteers}}. Use \code{gazetteers="all"} to load all available gazetteers
#' @param cache_directory string: (optional) cache the gazetteer data file locally in this directory, so that it can be used offline later. The cache directory will be created if it does not exist
#' @param refresh_cache logical: if TRUE, and a data file already exists in the cache_directory, it will be refreshed. If FALSE, the cached copy will be used
#' @param verbose logical: show progress messages?
#'
#' @return A data.frame
#'
#' @examples
#' \dontrun{
#'  g <- an_read(cache_directory="c:/temp/gaz")
#' }
#'
#' @export
an_read <- function(gazetteers="all",cache_directory,refresh_cache=FALSE,verbose=FALSE) {
    assert_that(is.flag(refresh_cache))
    assert_that(is.flag(verbose))
    ## currently the gazetteers parameter does nothing, since we only have the CGA to load
    do_cache_locally <- FALSE
    local_file_name <- "gaz_data.csv"
    ##download_url <- "https://data.aad.gov.au/geoserver/aadc/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=aadc:SCAR_CGA_PLACE_NAMES_NEW_SIMPLIFIED&outputFormat=csv"
    ## or for the complete set of columns
    download_url <- "https://data.aad.gov.au/geoserver/aadc/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=aadc:SCAR_CGA_PLACE_NAMES_NEW&outputFormat=csv"
    if (!missing(cache_directory)) {
        assert_that(is.string(cache_directory))
        if (!dir.exists(cache_directory)) {
            if (verbose) cat("creating data cache directory: ",cache_directory,"\n")
            ok <- dir.create(cache_directory)
            if (!ok) stop("could not create cache directory: ",cache_directory)
            do_cache_locally <- TRUE
        } else {
            ## cache dir exists
            ## does data file exist
            local_file_name <- file.path(cache_directory,local_file_name)
            if (refresh_cache || !file.exists(local_file_name)) do_cache_locally <- TRUE
        }
    } else {
        ## just provide the URL, and read_csv will fetch it
        local_file_name <- download_url
    }
    if (do_cache_locally) {
        if (verbose) cat("downloading gazetter data file to ",local_file_name," ...")
        download.file(download_url,destfile=local_file_name,quiet=!verbose)
        if (verbose) cat("done.\n")
    } else {
        if (verbose) {
            if (local_file_name==download_url)
                cat("fetching gazetteer data from: ",local_file_name,"\n")
            else
                cat("using cached copy of gazetteer data: ",local_file_name,"\n")
        }
    }
    if (local_file_name==download_url) {
        ## fetch using httr::GET, because read_csv chokes on SSL errors
        ## download.file (above) doesn't seem to care about SSL errors!
        suppressMessages(g <- httr::content(httr::GET(download_url,httr::config(ssl_verifypeer = 0L)),as="parsed"))
    } else {
        suppressMessages(g <- readr::read_csv(local_file_name))
    }
    ## rename "gazetteer" to "cga_source_gazetteer"
    names(g)[names(g)=="gazetteer"] <- "cga_source_gazetteer"
    ## add "gazetteer" column (meaning the overall gazetteer name, cga in this case)
    g$gazetteer <- "cga"
    ## split display scales into separate columns
    temp <- lapply(g$display_scales,strsplit,split=",")
    all_scales <- Filter(nchar,unique(sapply(temp,function(z)z[[1]][[1]])))
    temp <- as.data.frame(lapply(all_scales,function(sc)sapply(temp,function(z)isTRUE(any(sc==z[[1]])))))
    names(temp) <- paste0("display_scale_",gsub("000$","k",gsub("000000$","M",all_scales))) ## to e.g. display_scale_250k
    bind_cols(g,temp)
}

#' @rdname an_read
#' @export
an_gazetteers <- function() c("cga") ## for now, the CGA is the only gazetteer provided

## internal function, used to control the subset of columns returned to the user
gaz_names_to_show <- function(gaz) intersect(names(gaz),c("FID","gaz_id","place_name","english_place_name","altitude","feature_type_name","narrative","gazetteer","cga_source_gazetteer","latitude","longitude","geometry","scar_common_id","country_name","country_id",names(gaz)[grep("^display_scale_",names(gaz))]))
