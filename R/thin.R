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
