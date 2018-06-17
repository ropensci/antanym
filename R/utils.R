#' Calculate approximate map scale
#'
#' @param map_dimensions numeric: 2-element numeric giving width and height of the map, in mm
#' @param map_extent vector of c(longitude_min, longitude_max, latitude_min, latitude_max): the geographic extent of the map. \code{map_extent} can also be passed as a raster Extent object, a Raster object (in which case its extent will be used), a Spatial object (in which case the bounding box of the object will be used as the extent), or a matrix (in which case it will be assumed to be the output of \code{sp::bbox})
#'
#' @return numeric
#'
#' @examples
#' ## an A3-sized map of the Southern Ocean (1:20M)
#' an_mapscale(map_dimensions = c(400, 570), map_extent = c(-180, 180, -90, -40))
#'
#' @export
an_mapscale <- function(map_dimensions, map_extent) {
    assert_that((is.numeric(map_extent) && length(map_extent) == 4) || inherits(map_extent, "Extent") || inherits(map_extent, "Raster") || inherits(map_extent, "Spatial"))
    ## make sure map_extent is an Extent object
    if (inherits(map_extent, "Raster")) map_extent <- raster::extent(map_extent)
    if (inherits(map_extent, "Spatial")) map_extent <- sp::bbox(map_extent)
    if (inherits(map_extent, "matrix")) {
        ## if matrix, assume is an sp::bbox object
        map_extent <- as.numeric(map_extent)
        ## this is ordered c(xmin, ymin, xmax, ymax)
        map_extent <- map_extent[c(1, 3, 2, 4)]
    }
    if (!inherits(map_extent, "Extent")) map_extent <- raster::extent(as.numeric(map_extent))
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
