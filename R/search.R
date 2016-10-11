#' Find placenames near a given location
#'
#' @references \url{http://www.scar.org/data-products/cga}
#' @param cga SQLiteDriver: as returned by \code{\link{load_cga}}
#' @param loc numeric: longitude and latitude of target location
#' @param max_distance numeric: maximum search distance in kilometres
#'
#' @return data.frame of results
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


#' Search for place names
#'
#' @references \url{http://www.scar.org/data-products/cga}
#' @param cga SQLiteDriver: as returned by \code{\link{load_cga}}
#' @param query string: SQL-syntax search string. Matches are case-insensitive
#' @param feature_type string: if provided, search only for place names corresponding to features of this type
#' @param origin_country string: if provided, search only for place names originating from this country. For valid entries see \code{\link{cga_countries}}
#' @param origin_gazetteer string: if provided, search only for place names originating from this source gazetteer. For valid entries see \code{\link{cga_gazetteers}}
#'
#' @return data.frame of results
#'
#' @seealso \code{\link{load_cga}}
#'
#' @examples
#' \dontrun{
#'  g <- load_cga(cache_directory="c:/temp/cga")
#'  search_names(g,"Ufs%")
#'  search_names(g,"Ufs%",feature_type="Island")
#'  search_names(g,"Ufs%",feature_type="Island",origin_country="Australia")
#' }
#' @export
search_names <- function(cga,query,feature_type,origin_country,origin_gazetteer) {
    name_match_op <- ifelse(grepl("%",query),"LIKE","=")
    sstr <- sprintf("select * from cga where lower(place_name) %s '%s'",name_match_op,tolower(query))
    where <- c()
    if (!missing(feature_type))
        where <- c(where,sprintf("feature_type_name='%s'",feature_type))
    if (!missing(origin_country))
        where <- c(where,sprintf("country_name='%s'",origin_country))
    if (!missing(origin_gazetteer))
        where <- c(where,sprintf("gazetteer='%s'",origin_gazetteer))
    if (length(where)) sstr <- paste0(sstr," and ",paste(where,collapse=" and "))
    dbGetQuery(cga,sstr)
}

#' @rdname search_names
cga_countries <- function(cga) {
    as.character(na.omit(dbGetQuery(cga,sprintf("select distinct country_name from cga order by country_name"))$country_name))
}

#' @rdname search_names
cga_gazetteers <- function(cga) {
    as.character(na.omit(dbGetQuery(cga,sprintf("select distinct gazetteer from cga order by gazetteer"))$gazetteer))
}


