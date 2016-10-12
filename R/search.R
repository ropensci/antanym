#' Find placenames near a given location
#'
#' @references \url{http://www.scar.org/data-products/cga}
#' @param cga data.frame: as returned by \code{\link{agn_read}}
#' @param loc numeric: longitude and latitude of target location
#' @param max_distance numeric: maximum search distance in kilometres
#'
#' @return data.frame of results
#'
#' @seealso \code{\link{agn_read}}
#'
#' @examples
#' \dontrun{
#'  g <- agn_read(cache_directory="c:/temp/cga")
#'  agn_near(g,c(110,-66),10)
#'
#'  ## using dplyr or magrittr
#'  g %>% agn_near(c(100,-66),20)
#' }
#'
#' @export
agn_near <- function(cga,loc,max_distance) {
    dist <- geosphere::distVincentySphere(loc,cga[,c("longitude","latitude")])/1e3
    dist[is.na(dist)] <- Inf
    cga[dist<=max_distance,cga_names_to_show()]
}


#' Subset place names by various criteria
#'
#' @references \url{http://www.scar.org/data-products/cga}
#' @param cga data.frame: as returned by \code{\link{agn_read}}
#' @param query string: regular expression to match on place name. Matches are case-insensitive
#' @param extent raster::extent object or vector of c(xmin,xmax,ymin,ymax): if provided, search only for names within this bounding box
#' @param feature_type string: if provided, search only for place names corresponding to features of this type. For valid values see \code{\link{cga_feature_types}}
#' @param origin_country string: if provided, search only for place names originating from this country. For valid values see \code{\link{cga_countries}}
#' @param origin_gazetteer string: if provided, search only for place names originating from this source gazetteer. For valid values see \code{\link{cga_gazetteers}}
#' @param display_scale string: if provided, search only for place names that have been marked for display at this map scale. For valid values see \code{\link{cga_display_scales}}
#'
#' @return data.frame of results
#'
#' @seealso \code{\link{agn_read}}
#'
#' @examples
#' \dontrun{
#'  g <- agn_read(cache_directory="c:/temp/cga")
#'  agn_filter(g,"Ufs")
#'  agn_filter(g,"Ufs",feature_type="Island")
#'  agn_filter(g,"Ufs",feature_type="Island",origin_country="Australia")
#'
#'  nms <- agn_filter(g,extent=c(100,120,-70,-65),display_scale="2000000",
#'     origin_country="Australia")
#'  with(nms,plot(longitude,latitude))
#'  with(nms,text(longitude,latitude,place_name))
#'
#'  ## using dplyr or magrittr
#'  g %>% agn_filter("Ross",feature_type="Ice shelf")
#'  g %>% agn_near(c(100,-66),20) %>% agn_filter(feature_type="Island")
#' }
#' @export
agn_filter <- function(cga,query,extent,feature_type,origin_country,origin_gazetteer,display_scale) {
    idx <- rep(TRUE,nrow(cga))
    out <- cga
    if (!missing(query))
        out <- filter_(out,~grepl(query,place_name,ignore.case=TRUE))
    if (!missing(extent)) {
        out <- filter_(out,~longitude>=extent[1] & longitude<=extent[2] & latitude>=extent[3] & latitude<=extent[4])
    }
    if (!missing(feature_type))
        out <- filter_(out,~feature_type_name==feature_type)
    if (!missing(origin_country))
        out <- filter_(out,~country_name==origin_country)
    if (!missing(origin_gazetteer))
        out <- filter_(out,~gazetteer==origin_gazetteer)
    if (!missing(display_scale)) {
        dscol <- paste0("_display_scale_",display_scale)
        cat(dscol,"\n")
        if (!dscol %in% names(cga)) stop("display_scale ",display_scale," not valid: see cga_display_scales()")
        out <- out[out[,dscol]==TRUE,]
    }
    out[,cga_names_to_show()]
}

#' @rdname agn_filter
cga_countries <- function(cga) {
    sort(na.omit(distinct_(cga,"country_name"))$country_name)
}

#' @rdname agn_filter
cga_feature_types <- function(cga) {
    sort(as.character(na.omit(distinct_(cga,"feature_type_name"))$feature_type_name))
}

#' @rdname agn_filter
cga_gazetteers <- function(cga) {
    sort(as.character(na.omit(distinct_(cga,"gazetteer"))$gazetteer))
}

#' @rdname agn_filter
cga_display_scales <- function(cga) {
    nms <- names(cga)
    sort(gsub("^_display_scale_","",nms[grep("^_display_scale",nms)]))
}

