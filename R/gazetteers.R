#' The place name gazetteers available
#'
#' Return a character vector that lists all of the gazetteers available through the antanym package. Currently only one gazetteer is available: the SCAR Composite Gazetteer of Antarctica.
#'
#' @return character vector
#'
#' @seealso \code{\link{an_read}}, \code{\link{an_filter}}
#'
#' @examples
#'
#' an_gazetteers()
#'
#' @export
an_gazetteers <- function() {
    c("CGA") ## for now, the CGA is the only gazetteer provided
}
