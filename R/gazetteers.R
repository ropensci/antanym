#' The place name gazetteers available
#'
#' Return a character vector that lists all of the gazetteers present in the \code{gaz} data, or (if \code{gaz} was not provided) all of the gazetteers available through the antanym package. Currently only one gazetteer is available: the Composite Gazetteer of Antarctica.
#'
#' @param gaz data.frame or SpatialPointsDataFrame: (optional) as returned by \code{\link{an_read}}, \code{\link{an_preferred}}, or \code{\link{an_filter}}
#'
#' @return character vector. If \code{gaz} was provided, this will be a list of all gazetteers present in \code{gaz}. Otherwise, it will be a list of all gazetteers available through the antanym package
#'
#' @seealso \code{\link{an_read}}, \code{\link{an_filter}}
#'
#' @examples
#'
#' an_gazetteers()
#'
#' \dontrun{
#'  g <- an_read(cache = "session")
#'  an_gazetteers(g)
#' }
#'
#' @export
an_gazetteers <- function(gaz) {
    if (!missing(gaz)) {
        assert_that(inherits(gaz, c("data.frame", "SpatialPointsDataFrame")))
        unique(gaz$gazetteer)
    } else {
        c("CGA") ## for now, the CGA is the only gazetteer provided
    }
}
