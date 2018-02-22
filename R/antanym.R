#' \pkg{antanym}
#'
#' Antarctic geographic place names from the SCAR Composite Gazetteer of Antarctica, and functions for working with those place names.
#'
#' @name antanym
#' @aliases antanym-package
#' @docType package
#' @references \url{http://data.aad.gov.au/aadc/gaz/scar}
#' @importFrom assertthat assert_that is.string is.flag
#' @importFrom dplyr %>% arrange bind_cols bind_rows desc filter group_by slice ungroup
#' @importFrom geosphere distVincentySphere
#' @importFrom httr GET config content
#' @importFrom readr read_csv
#' @importFrom rlang .data
#' @importFrom sp coordinates coordinates<- CRS is.projected spTransform SpatialPoints
#' @import spdplyr
#' @importFrom stats dist na.omit predict
#' @importFrom utils download.file
#' @importFrom raster area cellStats crop extent extent<- projectExtent projection<- raster
#' @importFrom stringi stri_trans_general
#' @importFrom C50 predict.C5.0
NULL
