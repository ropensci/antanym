#' \pkg{antanym}
#'
#' @name antanym
#' @author Australian Antarctic Data Centre \email{aadc@@aad.gov.au}
#' @docType package
#' @references \url{http://data.aad.gov.au/aadc/gaz/scar}
#' @importFrom assertthat assert_that is.string is.flag
#' @importFrom dplyr %>% arrange_ bind_cols bind_rows distinct_ filter_ group_by_ select_ slice
#' @importFrom geosphere distVincentySphere
#' @importFrom httr GET config content
#' @importFrom readr read_csv
#' @importFrom stats dist na.omit predict
#' @importFrom utils download.file
#' @importFrom raster area cellStats extent extent<- raster
#' @importFrom stringi stri_trans_general
#' @importFrom C50 predict.C5.0
NULL
