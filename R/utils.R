#' Calculate approximate map scale
#'
#' @param map_dimensions numeric: 2-element numeric giving width and height of the map, in mm
#' @param map_extent raster Extent object or vector of c(longitude_min, longitude_max, latitude_min, latitude_max): the geographic extent of the map
#'
#' @return numeric
#'
#' @examples
#' ## an A3-sized map of the Southern Ocean (1:20M)
#' an_mapscale(map_dimensions = c(400, 570), map_extent = c(-180, 180, -90, -40))
#'
#' @export
an_mapscale <- function(map_dimensions, map_extent) {
    assert_that((is.numeric(map_extent) && length(map_extent) == 4) || inherits(map_extent, "Extent"))
    if (!inherits(map_extent, "Extent")) map_extent <- extent(as.numeric(map_extent))
    mapext <- raster()
    extent(mapext) <- map_extent

    sqrt(cellStats(area(mapext), "sum") * 1e3 * 1e3)/ ## sqrt of (map area in m^2)
        sqrt(prod(map_dimensions) / 1e3 / 1e3) ## sqrt of (map dimension area in m^2)
}


#' Get links to gazetteer entries
#'
#' Each entry in the Composite Gazetteer of Antarctica has its own web page. The \code{an_url} function will return the URL of the page associated with a given gazetteer entry.
#'
#' @references \url{https://www.scar.org/data-products/place-names/}
#' @param gaz data.frame or SpatialPointsDataFrame: as returned by \code{\link{an_read}}, \code{\link{an_preferred}}, or \code{\link{an_filter}}
#'
#' @return character vector, where each component is a URL to a web page giving more information about the associated gazetteer entry
#'
#' @examples
#' \dontrun{
#'  g <- an_read(cache = "session")
#'  my_url <- an_get_url(an_filter(g, query = "Ufs Island")[1, ])
#'  browseURL(my_url)
#' }
#' @export
an_get_url <- function(gaz) {
    assert_that(inherits(gaz, c("data.frame", "SpatialPointsDataFrame")))
    ## only CGA entries dealt with: needs modification once other gazetteers are added
    out <- rep(NA_character_, nrow(gaz))
    cga_idx <- gaz$gazetteer == "CGA"
    out[cga_idx] <- sprintf("https://data.aad.gov.au/aadc/gaz/scar/display_name.cfm?gaz_id=%d", gaz$gaz_id[cga_idx])
    out
}
