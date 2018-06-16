#' Thin names to give approximately uniform spatial coverage
#'
#' The provided data.frame of names will be thinned down to a smaller number of names. The thinning process attempts to select a subset of names that are uniformly spatially distributed, while simultaneously choosing the most important names (according to their relative score in the \code{score_col} column.
#'
#' Note that the algorithm calculates all pairwise distances between the rows of \code{gaz}. This is memory-intensive, and so if \code{gaz} has many rows the algorithm will fail or on some platforms might crash. Input \code{gaz} data.frames with more than \code{row_limit} rows will not be processed for this reason. You can try increasing \code{row_limit} from its default value if necessary.
#'
#' @param gaz data.frame or SpatialPointsDataFrame: typically as returned by \code{\link{an_suggest}}
#' @param n numeric: number of names to return
#' @param score_col string: the name of the column that gives the relative score of each name (e.g. as returned by \code{an_suggest}). Names with higher scores will be
#' preferred by the thinning process. If the specified \code{score_col} column is not present in \code{gaz}, or if all values within that column are equal, then the
#' thinning will be based entirely on the spatial distribution of the features
#' @param score_weighting numeric: weighting of scores relative to spatial distribution. A lower \code{score_weighting} value will tend to choose lower-scored names
#' in order to achieve better spatial uniformity. A higher \code{score_weighting} value will trade spatial uniformity in favour of selecting
#' higher-scored names
#' @param row_limit integer: the maximum number of rows allowed in \code{gaz}; see Details. Data frames larger than this will not be processed (with an error).
#'
#' @return data.frame
#'
#' @seealso \code{\link{an_read}}, \code{\link{an_suggest}}
#'
#' @examples
#' \dontrun{
#'  g <- an_read(cache = "session")
#'
#'  ## get a single name per feature, preferring the
#'  ##  Japanese name where there is one
#'  g <- an_preferred(g, origin = "Japan")
#'
#'  ## suggested names for a 100x100 mm map covering 60-90E, 70-60S
#'  ##  (this is about a 1:12M scale map)
#'  suggested <- an_suggest(g, map_extent = c(60, 90, -70, -60), map_dimensions = c(100, 100))
#'
#'  ## find the top 20 names by score
#'  head(suggested, 20)
#'
#'  ## find the top 20 names chosen for spatial coverage and score
#'  an_thin(suggested, 20)
#' }
#'
#' @export
an_thin <- function(gaz, n, score_col = "score", score_weighting = 5, row_limit=2000){
    assert_that(inherits(gaz, c("data.frame", "SpatialPointsDataFrame")))
    ## thin points to give approximately uniform spatial coverage
    ## optionally including scores
    if (n >= nrow(gaz)) return(gaz)
    if (nrow(gaz) > row_limit) stop("the input gaz data.frame has more rows than row_limit")
    idx <- rep(FALSE, nrow(gaz))
    ## construct matrix of distances between all pairs of points
    ## note that for computational reasons we use Euclidean distance on coordinates
    ## (even if the coords are long/lat, which would ideally be better dealt with using great-circle distances)
    if (inherits(gaz, "SpatialPointsDataFrame")) {
        this.dist <- as.matrix(dist(coordinates(gaz)))
    } else {
        this.dist <- as.matrix(dist(gaz[, c("longitude", "latitude")]))
    }
    if (!is.null(score_col) && score_col %in% names(gaz)) {
        sc <- gaz[[score_col]]
    } else {
        sc <- rep(1, nrow(gaz))
    }
    if (sum(!is.na(sc))<n) stop("there are less non-NA score values (", sum(!is.na(sc)),
                                ") than the requested number of names (", n, ")")
    tmp <- which.max(sc)
    idx[tmp] <- TRUE ## start with the first best-scored point
    sc[tmp] <- NA_real_
    while(sum(idx) < n) {
        ## rank the distances
        ## for each point, find its distance to the closest point that's already been selected
        dist_rank <- rank(apply(this.dist[, idx, drop=FALSE], 1, min), na.last = "keep")
        ## rank the scores
        score_rank <- rank(sc, na.last = "keep")
        ## select the point with highest avg rank (i.e. highest composite distance and score)
        tmp <- which.max(score_weighting * score_rank + dist_rank)
        idx[tmp] <- TRUE
        sc[tmp] <- NA_real_
    }
    gaz[idx, ]
}


#' Suggest names for a map (experimental)
#'
#' Features are given a suitability score based on how often (and at what map scales) they have been named on maps prepared by expert cartographers.
#' This is an experimental function and currently only implemented for \code{map_scale} values of 10 million or larger.
#'
#' @param gaz data.frame or SpatialPointsDataFrame: as returned by \code{\link{an_read}}, \code{\link{an_preferred}}, or \code{\link{an_filter}}
#' @param map_scale numeric: the scale of the map (e.g. 20e6 for a 1:20M map). If \code{map_scale} is not provided, it will be estimated from \code{map_extent} and \code{map_dimensions}
#' @param map_extent raster Extent object or vector of c(longitude_min, longitude_max, latitude_min, latitude_max): the extent of the area for which name suggestions are sought. Not required if \code{map_scale} is provided
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
#'
#'  suggested <- an_suggest(g, map_extent = c(60, 90, -70, -60), map_dimensions = c(100, 100))
#'  head(suggested, 20) ## top 20 names
#' }
#' @export
an_suggest <- function(gaz, map_scale, map_extent, map_dimensions) {
    assert_that(inherits(gaz, c("data.frame", "SpatialPointsDataFrame")))
    if (!missing(map_extent))
        assert_that((is.numeric(map_extent) && length(map_extent) == 4) || inherits(map_extent, "Extent"))

    if (missing(map_scale)) {
        if (missing(map_extent) || missing(map_dimensions)) stop("need either map_scale, or map_dimensions and map_extent")
        map_scale <- an_mapscale(map_dimensions, map_extent)
    }
    temp <- an_filter(gaz, extent = map_extent)
    if (map_scale>=10e6) {
        ## for scale >= 10e6: we have full coverage (nearly so for 12mill) of all scar_common_ids,
        ## so we use per-feature predictions. That is, for every feature we have a regression tree
        ## that models the probability of this feature being shown on a map of given scale
        ## the uid_fits object lives in sysdata.rda
        temp$score <- 0
        temp$scale <- map_scale
        idx <- which(temp$scar_common_id %in% uid)
        for (i in idx) {
            fidx <- which(temp$scar_common_id[i] == uid)
            if (inherits(uid_fits[[fidx]], "C5.0")) {
                temp$score[i] <- predict(uid_fits[[fidx]], newdata = temp[i,], type = "prob")[, 2]
            } else {
                temp$score[i] <- uid_fits[[fidx]]
            }
        }
    } else {
        ## for scale < 10e6: we do not have information on every feature at all scales
        ## so we have to build models that predict the likelihood of a given feature
        ## being shown on a given map, depending on the properties of the feature and
        ## the scale (and perhaps other properties) of the map
        stop("an_suggest not yet implemented for map_scale value below 10 million")
    }
    oidx <- order(temp$score, decreasing = TRUE)
    temp[oidx, ]
}


#' Calculate approximate map scale
#'
#' @param map_dimensions numeric: 2-element numeric giving width and height of the map, in mm
#' @param map_extent raster Extent object or vector of c(longitude_min, longitude_max, latitude_min, latitude_max): the geographic extent of the map
#'
#' @return numeric
#'
#' @examples
#' ## an A3-sized map of the Southern Ocean (1:20M)
#' an_mapscale(c(400, 570), c(-180, 180, -90, -40))
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
#' @references \url{http://www.scar.org/data-products/cga}
#' @param gaz data.frame or SpatialPointsDataFrame: as returned by \code{\link{an_read}}, \code{\link{an_preferred}}, or \code{\link{an_filter}}
#'
#' @return character vector, where each component is a URL to a web page giving more information about the associated gazetteer entry
#'
#' @examples
#' \dontrun{
#'  g <- an_read(cache = "session")
#'  my_url <- an_get_url(an_filter(g, "Ufs Island")[1, ])
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
