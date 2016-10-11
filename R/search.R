#' Find placenames near a given location
#'
#' @references \url{http://www.scar.org/data-products/cga}
#' @param cga data.frame: as returned by \code{\link{load_cga}}
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
#'  g %>% names_near(c(100,-66),20)
#' }
#'
#' @export
names_near <- function(cga,loc,max_distance) {
    dist <- geosphere::distVincentySphere(loc,cga[,c("longitude","latitude")])/1e3
    dist[is.na(dist)] <- Inf
    cga[dist<=max_distance,cga_names_to_show()]
}


#' Subset place names by various criteria
#'
#' @references \url{http://www.scar.org/data-products/cga}
#' @param cga data.frame: as returned by \code{\link{load_cga}}
#' @param query string: regular expression to match on place name. Matches are case-insensitive
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
#'  subset_names(g,"Ufs")
#'  subset_names(g,"Ufs",feature_type="Island")
#'  subset_names(g,"Ufs",feature_type="Island",origin_country="Australia")
#'
#'  nms <- subset_names(gg,extent=c(100,120,-70,-65),display_scale="2000000",
#'     origin_country="Australia")
#'  with(nms,plot(longitude,latitude))
#'  with(nms,text(longitude,latitude,place_name))
#'
#'  ## using dplyr or magrittr
#'  g %>% subset_names("Ross",feature_type="Ice shelf")
#'  g %>% names_near(c(100,-66),20) %>% subset_names(feature_type="Island")
#' }
#' @export
subset_names <- function(cga,query,extent,feature_type,origin_country,origin_gazetteer,display_scale) {
    idx <- rep(TRUE,nrow(cga))
    out <- cga
    if (!missing(query))
        out <- filter(out,grepl(query,out$place_name,ignore.case=TRUE))
    if (!missing(extent)) {
        out <- filter(out,longitude>=extent[1] & longitude<=extent[2] & latitude>=extent[3] & latitude<=extent[4])
    }
    if (!missing(feature_type))
        out <- filter(out,feature_type_name==feature_type)
    if (!missing(origin_country))
        out <- filter(out,out$country_name==origin_country)
    if (!missing(origin_gazetteer))
        out <- filter(out,out$gazetteer==origin_gazetteer)
    if (!missing(display_scale)) {
        dscol <- paste0("_display_scale_",display_scale)
        if (!dscol %in% names(cga)) stop("display_scale ",display_scale," not valid: see cga_display_scales()")
        out <- out[out[,dscol],]
    }
    out[,cga_names_to_show()]
}

#' @rdname subset_names
cga_countries <- function(cga) {
    sort(na.omit(distinct_(cga,"country_name"))$country_name)
}

#' @rdname subset_names
cga_feature_types <- function(cga) {
    sort(as.character(na.omit(distinct_(cga,"feature_type_name"))$feature_type_name))
}

#' @rdname subset_names
cga_gazetteers <- function(cga) {
    sort(as.character(na.omit(distinct_(cga,"gazetteer"))$gazetteer))
}

#' @rdname subset_names
cga_display_scales <- function(cga) {
    nms <- names(cga)
    sort(gsub("^_display_scale_","",nms[grep("^_display_scale",nms)]))
}

