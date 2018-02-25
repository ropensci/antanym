#' Find placenames near a given location
#'
#' @references \url{http://www.scar.org/data-products/cga}
#' @param gaz data.frame or SpatialPointsDataFrame: as returned by \code{\link{an_read}}
#' @param loc numeric: target location (a two-element numeric vector giving longitude and latitude, or a SpatialPoints object)
#' @param max_distance numeric: maximum search distance in kilometres
#'
#' @return data.frame of results
#'
#' @seealso \code{\link{an_read}}
#'
#' @examples
#' \dontrun{
#'  g <- an_read(cache_directory="c:/temp/gaz")
#'
#'  ## named features within 10km of 110E, 66S
#'  an_near(g, c(110, -66), 10)
#'
#'  ## using pipe operator
#'  g %>% an_near(c(100, -66), 10)
#'
#'  ## with sp objects
#'  gsp <- an_read(cache_directory = "c:/temp/gaz", sp = TRUE)
#'  loc <- SpatialPoints(matrix(c(110, -66), nrow = 1),
#'    proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))
#'  an_near(gsp, loc, 10)
#' }
#'
#' @export
an_near <- function(gaz, loc, max_distance) {
    ## make sure gaz locations are in longitude and latitude
    if (inherits(gaz, "SpatialPointsDataFrame")) {
        tmp <- as.data.frame(gaz)
        if (!is.projected(gaz)) {
            crds <- tmp[,c("longitude", "latitude")]
        } else {
            crds <- coordinates(spTransform(gaz, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84")))
        }
    } else {
        crds <- gaz[,c("longitude", "latitude")]
    }
    ## make sure loc is in longitude and latitude
    if (inherits(loc, "SpatialPoints")) {
        if (is.projected(loc)) loc <- coordinates(spTransform(loc, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84")))
    }
    dist <- geosphere::distVincentySphere(loc, crds)/1e3
    dist[is.na(dist)] <- Inf
    gaz[dist<=max_distance,]
}


#' Filter a collection of place names by various criteria
#'
#' A data.frame of place names can be filtered according to name, geographic location, feature type, or other criteria. All text-related matches are treated as regular expressions and are case-insensitive.
#'
#' @references \url{http://www.scar.org/data-products/cga}
#' @param gaz data.frame or SpatialPointsDataFrame: as returned by \code{\link{an_read}}
#' @param query string: return only place names matching this pattern (regular expression)
#' @param feature_ids numeric: return only place names associated with the features identified by these identifiers. Currently these values can only be \code{scar_common_id} values
#' @param extent raster Extent object or vector of c(longitude_min, longitude_max, latitude_min, latitude_max): if provided, search only for names within this bounding box
#' @param feature_type string: return only place names corresponding to feature types matching this pattern (regular expression). For valid feature type names see \code{\link{an_feature_types}}
#' @param origin_country string: return only names originating from countries matching this pattern (regular expression). For valid country names see \code{\link{an_countries}}
#' @param origin_gazetteer string: return only place names originating from gazetteers matching this pattern (regular expression). For valid gazetteer names see \code{\link{an_gazetteers}}
#' @param cga_source string: return only place names in the SCAR Composite Gazetteer originating from contributing gazetteers matching this pattern (regular expression). For valid CGA source gazetteer names see \code{\link{an_cga_sources}}
#'
#' @return data.frame of results
#'
#' @seealso \code{\link{an_read}}, \code{\link{an_gazetteers}}
#'
#' @examples
#' \dontrun{
#'  g <- an_read(cache_directory="c:/temp/gaz")
#'  an_filter(g, "Ufs")
#'  an_filter(g, "Ufs", feature_type = "Island")
#'  an_filter(g, "Ufs", feature_type = "Island", origin_country = "Australia|United States of America")
#'
#'  nms <- an_filter(g, extent = c(100, 120, -70, -65),
#'     origin_country = "Australia")
#'  with(nms, plot(longitude, latitude))
#'  with(nms, text(longitude, latitude, place_name))
#'
#'  ## using pipe operator
#'  g %>% an_filter("Ross", feature_type = "Ice shelf|Mountain")
#'  g %>% an_near(c(100, -66), 20) %>% an_filter(feature_type = "Island")
#'
#'  ## all names for feature 1589 and the country that issued the name
#'  an_filter(g, feature_ids = 1589)[,c("place_name", "country_name")]
#' }
#' @export
an_filter <- function(gaz, query, feature_ids, extent, feature_type, origin_country, origin_gazetteer, cga_source) {
    idx <- rep(TRUE, nrow(gaz))
    if (!missing(feature_ids)) {
        idx <- gaz$scar_common_id %in% feature_ids
    }
    if (!missing(query)) {
        assert_that(is.string(query))
        ## split query into words, and match against each
        sterms <- strsplit(query, "[ ,]+")[[1]]
        for (st in sterms) idx <- idx & (grepl(st, gaz$place_name, ignore.case = TRUE) | grepl(st, gaz$place_name_transliterated, ignore.case = TRUE))
    }
    out <- gaz[idx,]
    if (!missing(extent)) {
        assert_that((is.numeric(extent) && length(extent)==4) || inherits(extent, "Extent"))
        if (inherits(out, "SpatialPointsDataFrame")) {
            out <- crop(out, extent)
        } else {
            out <- out[out$longitude>=extent[1] & out$longitude<=extent[2] & out$latitude>=extent[3] & out$latitude<=extent[4],]
        }
    }
    if (!missing(feature_type)) {
        assert_that(is.string(feature_type))
        out <- out[grepl(feature_type, out$feature_type_name),]
    }
    if (!missing(origin_country)) {
        assert_that(is.string(origin_country))
        out <- out[grepl(origin_country, out$country_name),]
    }
    if (!missing(origin_gazetteer)) {
        assert_that(is.string(origin_gazetteer))        
        out <- out[grepl(origin_gazetteer, out$gazetteer),]
    }
    if (!missing(cga_source)) {
        assert_that(is.string(cga_source))
        out <- out[!is.na(out$gazetteer) & out$gazetteer=="CGA" & grepl(cga_source, out$cga_source_gazetteer),]
    }
    out
}

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
    sort(as.character(na.omit(unique(gaz$cga_source_gazetteer[!is.na(gaz$gazetteer) & gaz$gazetteer=="CGA"]))))
}


