#' Find placenames near a given location
#'
#' @references \url{http://www.scar.org/data-products/cga}
#' @param gaz data.frame or SpatialPointsDataFrame: as returned by \code{\link{an_read}}
#' @param loc numeric: target location (numeric longitude and latitude, or SpatialPoints object)
#' @param max_distance numeric: maximum search distance in kilometres
#'
#' @return data.frame of results
#'
#' @seealso \code{\link{an_read}}
#'
#' @examples
#' \dontrun{
#'  g <- an_read(cache_directory="c:/temp/gaz")
#'  an_near(g,c(110,-66),10)
#'
#'  ## using dplyr or magrittr
#'  g %>% an_near(c(100,-66),20)
#'
#'  ## with sp objects
#'  gsp <- an_read(cache_directory="c:/temp/gaz",sp=TRUE)
#'  loc <- SpatialPoints(matrix(c(110,-66),nrow=1),proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))
#'  an_near(gsp,loc,10)
#' }
#'
#' @export
an_near <- function(gaz,loc,max_distance) {
    ## make sure gaz locations are in longitude and latitude
    if (inherits(gaz,"SpatialPointsDataFrame")) {
        tmp <- as.data.frame(gaz)
        if (!is.projected(gaz)) {
            crds <- tmp[,c("longitude","latitude")]
        } else {
            crds <- coordinates(spTransform(gaz,CRS("+proj=longlat +datum=WGS84 +ellps=WGS84")))
        }
    } else {
        crds <- gaz[,c("longitude","latitude")]
    }
    ## make sure loc is in longitude and latitude
    if (inherits(loc,"SpatialPoints")) {
        if (is.projected(loc)) loc <- coordinates(spTransform(loc,CRS("+proj=longlat +datum=WGS84 +ellps=WGS84")))
    }
    dist <- geosphere::distVincentySphere(loc,crds)/1e3
    dist[is.na(dist)] <- Inf
    gaz[dist<=max_distance,]
}


#' Subset place names by various criteria
#'
#' All matches are case-insensitive.
#'
#' @references \url{http://www.scar.org/data-products/cga}
#' @param gaz data.frame or SpatialPointsDataFrame: as returned by \code{\link{an_read}}
#' @param query string, regular expression: Return only place names matching this pattern
#' @param extent raster Extent object or vector of c(longitude_min,longitude_max,latitude_min,latitude_max): if provided, search only for names within this bounding box
#' @param feature_type string, regular expression: Return only place names corresponding to feature types matching this pattern. For valid feature type names see \code{\link{an_feature_types}}
#' @param origin_country string, regular expression: Return only names originating from this countries matching this pattern. For valid country names see \code{\link{an_countries}}
#' @param origin_gazetteer string, regular expression: Return only place names originating from gazetteers matching this pattern. For valid gazetteer names see \code{\link{an_gazetteers}}
#' @param cga_source string, regular expression: Return only place names in the SCAR Composite Gazetteer originating from contributing gazetteers matching this pattern. For valid CGA source gazetteer names see \code{\link{an_cga_sources}}. (Only applicable to the SCAR Composite Gazetteer)
#'
#' @return data.frame of results
#'
#' @seealso \code{\link{an_read}}
#'
#' @examples
#' \dontrun{
#'  g <- an_read(cache_directory="c:/temp/gaz")
#'  an_filter(g,"Ufs")
#'  an_filter(g,"Ufs",feature_type="Island")
#'  an_filter(g,"Ufs",feature_type="Island",origin_country="Australia|United States of America")
#'
#'  nms <- an_filter(g,extent=c(100,120,-70,-65),
#'     origin_country="Australia")
#'  with(nms,plot(longitude,latitude))
#'  with(nms,text(longitude,latitude,place_name))
#'
#'  ## using dplyr or magrittr
#'  g %>% an_filter("Ross",feature_type="Ice shelf|Mountain")
#'  g %>% an_near(c(100,-66),20) %>% an_filter(feature_type="Island")
#' }
#' @export
an_filter <- function(gaz,query,extent,feature_type,origin_country,origin_gazetteer,cga_source) {
    idx <- rep(TRUE,nrow(gaz))
    if (!missing(query)) {
        ## split query into words, and match against each
        sterms <- strsplit(query,"[ ,]+")[[1]]
        for (st in sterms) idx <- idx & (grepl(st,gaz$place_name,ignore.case=TRUE) | grepl(st,gaz$place_name_transliterated,ignore.case=TRUE))
    }
    out <- gaz[idx,]
    if (!missing(extent)) {
        assert_that((is.numeric(extent) && length(extent)==4) || inherits(extent,"Extent"))
        if (inherits(out,"SpatialPointsDataFrame")) {
            out <- crop(out,extent)
        } else {
            out <- filter_(out,~longitude>=extent[1] & longitude<=extent[2] & latitude>=extent[3] & latitude<=extent[4])
        }
    }
    if (!missing(feature_type))
        out <- filter_(out,~grepl(feature_type,feature_type_name))
    if (!missing(origin_country))
        out <- filter_(out,~grepl(origin_country,country_name))
    if (!missing(origin_gazetteer))
        out <- filter_(out,~grepl(origin_gazetteer,gazetteer))
    if (!missing(cga_source))
        out <- filter_(out,~gazetteer=="cga" & grepl(cga_source,cga_source_gazetteer))
    #if (!missing(display_scale)) {
    #    dscol <- paste0("display_scale_",display_scale)
    #    if (!dscol %in% names(gaz)) stop("display_scale ",display_scale," not valid: see an_display_scales()")
    #    out <- out[out[,dscol]==TRUE,]
    #}
    out
}
# @param display_scale string: Return only place names that have been marked for display at this map scale. For valid values see \code{\link{an_display_scales}}


#' @rdname an_filter
#' @export
an_countries <- function(gaz) {
    sort(na.omit(unique(gaz$country_name)))
}

#' @rdname an_filter
#' @export
an_feature_types <- function(gaz) {
    sort(as.character(na.omit(unique(gaz$feature_type_name))))
}

#' @rdname an_filter
#' @export
an_cga_sources <- function(gaz) {
    sort(as.character(na.omit(unique(filter_(gaz,~gazetteer=="cga")$cga_source_gazetteer))))
}

# ' @rdname an_filter
# ' @export
#an_display_scales <- function(gaz) {
#    nms <- names(gaz)
#    sort(gsub("^display_scale_","",nms[grep("^display_scale_",nms)]))
#}

