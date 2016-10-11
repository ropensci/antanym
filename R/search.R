#' Find placenames near a given location
#'
#' @references \url{http://www.scar.org/data-products/cga}
#' @param cga SQLiteConnection: as returned by \code{\link{load_cga}}
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
#'
#'  ## using dplyr or magrittr
#'  gg %>% names_near(c(100,-66),10)
#' }
#'
#' @export
names_near <- function(cga,loc,max_distance) {
    ## this of course should be done as a spatial query! but for now:
    temp <- dbGetQuery(cga,"select longitude,latitude,gaz_id from cga")
    dist <- geosphere::distVincentySphere(loc,temp)/1e3
    dbGetQuery(cga,sprintf("select * from cga where gaz_id in (%s)",paste(na.omit(temp$gaz_id[dist<=max_distance]),collapse=",")))[,cga_names_to_show()]
}


#' Search for place names
#'
#' @references \url{http://www.scar.org/data-products/cga}
#' @param cga SQLiteConnection: as returned by \code{\link{load_cga}}
#' @param query string: SQL-syntax search string. Matches are case-insensitive
#' @param extent raster::extent object or vector of c(xmin,xmax,ymin,ymax): if provided, search only for names within this bounding box
#' @param feature_type string: if provided, search only for place names corresponding to features of this type. For valid values see \code{\link{cga_feature_types}}
#' @param origin_country string: if provided, search only for place names originating from this country. For valid values see \code{\link{cga_countries}}
#' @param origin_gazetteer string: if provided, search only for place names originating from this source gazetteer. For valid values see \code{\link{cga_gazetteers}}
#' @param display_scale string: if provided, search only for place names that have been marked for display at this map scale. For valid values see \code{\link{cga_display_scales}}
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
#'
#'  nms <- search_names(gg,extent=c(100,120,-70,-65),display_scale="2000000",
#'     origin_country="Australia")
#'  with(nms,plot(longitude,latitude))
#'  with(nms,text(longitude,latitude,place_name))
#'
#'  ## using dplyr or magrittr
#'  g %>% search_names("Ross%",feature_type="Ice shelf")
#' }
#' @export
search_names <- function(cga,query,extent,feature_type,origin_country,origin_gazetteer,display_scale) {
    sstr <- "select * from cga"
    where <- c()
    if (!missing(query)) {
        name_match_op <- ifelse(grepl("%",query),"LIKE","=")
        where <- c(where,sprintf("lower(place_name) %s '%s'",name_match_op,tolower(query)))
    }
    if (!missing(extent)) {
        where <- c(where,paste0("longitude>=",extent[1]))
        where <- c(where,paste0("longitude<=",extent[2]))
        where <- c(where,paste0("latitude>=",extent[3]))
        where <- c(where,paste0("latitude<=",extent[4]))
    }
    if (!missing(feature_type))
        where <- c(where,sprintf("feature_type_name='%s'",feature_type))
    if (!missing(origin_country))
        where <- c(where,sprintf("country_name='%s'",origin_country))
    if (!missing(origin_gazetteer))
        where <- c(where,sprintf("gazetteer='%s'",origin_gazetteer))
    if (!missing(display_scale)) {
        dscol <- paste0("_display_scale_",display_scale)
        nms <- names(dbGetQuery(cga,"select * from cga limit 0"))
        if (!dscol %in% nms) stop("display_scale ",display_scale," not valid: see cga_display_scales()")
        where <- c(where,sprintf("%s=1",dscol))
    }
    if (length(where)) sstr <- paste0(sstr," where ",paste(where,collapse=" and "))
    dbGetQuery(cga,sstr)[,cga_names_to_show()]
}

#' @rdname search_names
cga_countries <- function(cga) {
    sort(as.character(na.omit(dbGetQuery(cga,sprintf("select distinct country_name from cga order by country_name"))$country_name)))
}

#' @rdname search_names
cga_feature_types <- function(cga) {
    sort(as.character(na.omit(dbGetQuery(cga,sprintf("select distinct feature_type_name from cga order by country_name"))$feature_type_name)))
}

#' @rdname search_names
cga_gazetteers <- function(cga) {
    as.character(na.omit(dbGetQuery(cga,sprintf("select distinct gazetteer from cga order by gazetteer"))$gazetteer))
}

#' @rdname search_names
cga_display_scales <- function(cga) {
    nms <- names(dbGetQuery(cga,"select * from cga limit 0"))
    sort(gsub("^_display_scale_","",nms[grep("^_display_scale",nms)]))
}

