#' Find placenames near a given location
#'
#' @references \url{http://www.scar.org/data-products/cga}
#' @param gaz data.frame: as returned by \code{\link{agn_read}}
#' @param loc numeric: longitude and latitude of target location
#' @param max_distance numeric: maximum search distance in kilometres
#'
#' @return data.frame of results
#'
#' @seealso \code{\link{agn_read}}
#'
#' @examples
#' \dontrun{
#'  g <- agn_read(cache_directory="c:/temp/gaz")
#'  agn_near(g,c(110,-66),10)
#'
#'  ## using dplyr or magrittr
#'  g %>% agn_near(c(100,-66),20)
#' }
#'
#' @export
agn_near <- function(gaz,loc,max_distance) {
    dist <- geosphere::distVincentySphere(loc,gaz[,c("longitude","latitude")])/1e3
    dist[is.na(dist)] <- Inf
    gaz[dist<=max_distance,gaz_names_to_show(gaz)]
}


#' Subset place names by various criteria
#'
#' All matches are case-insensitive.
#'
#' @references \url{http://www.scar.org/data-products/cga}
#' @param gaz data.frame: as returned by \code{\link{agn_read}}
#' @param query string, regular expression: Return only place names matching this pattern
#' @param extent raster::extent object or vector of c(xmin,xmax,ymin,ymax): if provided, search only for names within this bounding box
#' @param feature_type string, regular expression: Return only place names corresponding to feature types matching this pattern. For valid feature type names see \code{\link{agn_feature_types}}
#' @param origin_country string, regular expression: Return only names originating from this countries matching this pattern. For valid country names see \code{\link{agn_countries}}
#' @param origin_gazetteer string, regular expression: Return only place names originating from gazetteers matching this pattern. For valid gazetteer names see \code{\link{agn_gazetteers}}
#' @param display_scale string: Return only place names that have been marked for display at this map scale. For valid values see \code{\link{agn_display_scales}}
#'
#' @return data.frame of results
#'
#' @seealso \code{\link{agn_read}}
#'
#' @examples
#' \dontrun{
#'  g <- agn_read(cache_directory="c:/temp/gaz")
#'  agn_filter(g,"Ufs")
#'  agn_filter(g,"Ufs",feature_type="Island")
#'  agn_filter(g,"Ufs",feature_type="Island",origin_country="Australia|United States of America")
#'
#'  nms <- agn_filter(g,extent=c(100,120,-70,-65),display_scale="2000000",
#'     origin_country="Australia")
#'  with(nms,plot(longitude,latitude))
#'  with(nms,text(longitude,latitude,place_name))
#'
#'  ## using dplyr or magrittr
#'  g %>% agn_filter("Ross",feature_type="Ice shelf|Mountain")
#'  g %>% agn_near(c(100,-66),20) %>% agn_filter(feature_type="Island")
#' }
#' @export
agn_filter <- function(gaz,query,extent,feature_type,origin_country,origin_gazetteer,display_scale) {
    idx <- rep(TRUE,nrow(gaz))
    out <- gaz
    if (!missing(query))
        out <- filter_(out,~grepl(query,place_name,ignore.case=TRUE))
    if (!missing(extent)) {
        out <- filter_(out,~longitude>=extent[1] & longitude<=extent[2] & latitude>=extent[3] & latitude<=extent[4])
    }
    if (!missing(feature_type))
        out <- filter_(out,~grepl(feature_type,feature_type_name))
    if (!missing(origin_country))
        out <- filter_(out,~grepl(origin_country,country_name))
    if (!missing(origin_gazetteer))
        out <- filter_(out,~grepl(origin_gazetteer,gazetteer))
    if (!missing(display_scale)) {
        dscol <- paste0("_display_scale_",display_scale)
        cat(dscol,"\n")
        if (!dscol %in% names(gaz)) stop("display_scale ",display_scale," not valid: see agn_display_scales()")
        out <- out[out[,dscol]==TRUE,]
    }
    out[,gaz_names_to_show(out)]
}

#' @rdname agn_filter
#' @export
agn_countries <- function(gaz) {
    sort(na.omit(distinct_(gaz,"country_name"))$country_name)
}

#' @rdname agn_filter
#' @export
agn_feature_types <- function(gaz) {
    sort(as.character(na.omit(distinct_(gaz,"feature_type_name"))$feature_type_name))
}

#' @rdname agn_filter
#' @export
agn_gazetteers <- function(gaz) {
    sort(as.character(na.omit(distinct_(gaz,"gazetteer"))$gazetteer))
}

#' @rdname agn_filter
#' @export
agn_display_scales <- function(gaz) {
    nms <- names(gaz)
    sort(gsub("^_display_scale_","",nms[grep("^_display_scale",nms)]))
}

