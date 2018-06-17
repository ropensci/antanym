#' Suggest names for a map (experimental)
#'
#' Features are given a suitability score based on maps prepared by expert cartographers. Data were tabulated from a collection of such maps, indicating for each feature whether it was named on a given map, along with details (such as scale) of the map. These data are used as the basis of a recommendation algorithm, which suggests the best features to name on a map given its properties (extent and scale). This is an experimental function and currently only implemented for \code{map_scale} values of 10 million or larger.
#'
#' @param gaz data.frame or SpatialPointsDataFrame: as returned by \code{\link{an_read}}, \code{\link{an_preferred}}, or \code{\link{an_filter}}
#' @param map_scale numeric: the scale of the map (e.g. 20e6 for a 1:20M map). If \code{map_scale} is not provided, it will be estimated from \code{map_extent} and \code{map_dimensions}
#' @param map_extent vector of c(longitude_min, longitude_max, latitude_min, latitude_max): the extent of the area for which name suggestions are sought. This is required if \code{map_scale} is not provided, and optional if \code{map_scale} is provided (if \code{map_extent} is provided in this situation then the \code{gaz} data frame will be filtered to this extent before the suggestion algorithm is applied; otherwise all names in \code{gaz} will be considered). \code{map_extent} can also be passed as a raster Extent object, a Raster object (in which case its extent will be used), a Spatial object (in which case the bounding box of the object will be used as the extent), or a matrix (in which case it will be assumed to be the output of \code{sp::bbox})
#' @param map_dimensions numeric: 2-element numeric giving width and height of the map, in mm. Not required if \code{map_scale} is provided
#'
#' @return data.frame of names with a "score" column added. Score values range from 0 to 1. The data frame will be sorted in descending score order. Names with higher scores are those that are suggested as the most suitable for display.
#'
#' @seealso \code{\link{an_read}} \code{\link{an_thin}}
#'
#' @examples
#' \dontrun{
#'  g <- an_read(cache = "session")
#'
#'  ## get a single name per feature, preferring the
#'  ##  Australian name where there is one
#'  g <- an_preferred(g, origin = "Australia")
#'
#'  ## suggested names for a 100x100 mm map covering 60-90E, 70-60S
#'  ##  (this is about a 1:12M scale map)
#'  suggested <- an_suggest(g, map_extent = c(60, 90, -70, -60), map_dimensions = c(100, 100))
#'  head(suggested, 20) ## top 20 names
#'
#'  ## an equivalent result can be achieved by supplying map scale and extent
#'  suggested <- an_suggest(g, map_scale = 12e6, map_extent = c(60, 90, -70, -60))
#' }
#' @export
an_suggest <- function(gaz, map_scale, map_extent, map_dimensions) {
    assert_that(inherits(gaz, c("data.frame", "SpatialPointsDataFrame")))
    if (!missing(map_extent))
        assert_that((is.numeric(map_extent) && length(map_extent) == 4) || inherits(map_extent, "Extent") || inherits(map_extent, "Raster") || inherits(map_extent, "Spatial"))
    if (missing(map_scale)) {
        if (missing(map_extent) || missing(map_dimensions)) stop("need either map_scale, or map_dimensions and map_extent")
        map_scale <- an_mapscale(map_dimensions, map_extent)
    }
    if (!missing(map_extent)) gaz <- an_filter(gaz, extent = map_extent)
    if (map_scale>=10e6) {
        ## for scale >= 10e6: we have full coverage (nearly so for 12mill) of all scar_common_ids,
        ## so we use per-feature predictions. That is, for every feature we have a regression tree
        ## that models the probability of this feature being shown on a map of given scale
        ## the uid_fits object lives in sysdata.rda
        gaz$score <- 0
        gaz$scale <- map_scale
        idx <- which(gaz$scar_common_id %in% uid)
        for (i in idx) {
            fidx <- which(gaz$scar_common_id[i] == uid)
            if (inherits(uid_fits[[fidx]], "C5.0")) {
                gaz$score[i] <- predict(uid_fits[[fidx]], newdata = gaz[i,], type = "prob")[, 2]
            } else {
                gaz$score[i] <- uid_fits[[fidx]]
            }
        }
    } else {
        ## for scale < 10e6: we do not have information on every feature at all scales
        ## so we have to build models that predict the likelihood of a given feature
        ## being shown on a given map, depending on the properties of the feature and
        ## the scale (and perhaps other properties) of the map
        stop("an_suggest not yet implemented for map_scale value below 10 million")
    }
    oidx <- order(gaz$score, decreasing = TRUE)
    gaz[oidx, ]
}
