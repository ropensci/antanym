#' Find placenames near a given location
#'
#' @references \url{http://www.scar.org/data-products/cga}
#' @param cga SQLiteDriver: as returned by \code{\link{load_cga}}
#' @param loc numeric: longitude and latitude of target location
#' @param max_distance numeric: maximum search distance in kilometres
#'
#' @return data.frame
#'
#' @seealso \code{\link{load_cga}}
#'
#' @examples
#' \dontrun{
#'  g <- load_cga(cache_directory="c:/temp/cga")
#'  names_near(g,c(110,-66),10)
#' }
#'
#' @export
names_near <- function(cga,loc,max_distance) {
    ## this of course should be done as a spatial query! but for now:
    temp <- dbGetQuery(cga,"select longitude,latitude,gaz_id from cga")
    dist <- geosphere::distVincentySphere(loc,temp)/1e3
    dbGetQuery(cga,sprintf("select * from cga where gaz_id in (%s)",paste(na.omit(temp$gaz_id[dist<=max_distance]),collapse=",")))
}

