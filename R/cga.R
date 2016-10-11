#' \pkg{cga}
#'
#' @name cga
#' @author Australian Antarctic Data Centre \email{aadc@@aad.gov.au}
#' @docType package
#' @references \url{http://data.aad.gov.au/aadc/gaz/scar}
#' @importFrom readr read_csv
#' @importFrom assertthat assert_that is.string is.flag
#' @importFrom RSQLite SQLite dbConnect dbGetQuery dbWriteTable
#' @importFrom geosphere distVincentySphere
#' @importFrom stats na.omit
#' @importFrom utils download.file
NULL
