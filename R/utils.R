#' Find one name per feature
#'
#' The SCAR Composite Gazetteer of Antarctic is a compilation of place names provided by many different countries. The composite nature of the CGA means that there may be multiple names associated with a single feature. The \code{an_preferred} function can be used to resolve a single name per feature.
#'
#' @references \url{http://www.scar.org/data-products/cga}
#' @param gaz data.frame or SpatialPointsDataFrame: as returned by \code{\link{an_read}}
#' @param origin_country character: vector of country names, in order of preference. If a given feature has been named by one of these countries, this place name will be chosen. If the feature in question has not been given a name by one of these countries, a place name given by another country will be chosen. For valid \code{origin_country} values, see \code{\link{an_countries}}
#'
#' @return data.frame of results
#'
#' @seealso \code{\link{an_read}}, \code{\link{an_countries}}
#'
#' @examples
#' \dontrun{
#'  g <- an_read(cache_directory = "c:/temp/gaz")
#'
#'  ## get a single name per feature, preferring the
#'  ##  Polish name where there is one
#'
#'  pnames <- an_preferred(g, "Poland")
#'
#'  ## names starting with "Sm", preferring US names then
#'  ##  Australian ones if available
#'
#'  g %>% an_filter("^Sm") %>%
#'    an_preferred(origin_country = c("United States of America", "Australia"))
#' }
#'
#' @export
an_preferred <- function(gaz, origin_country) {
    assert_that(is.character(origin_country))
    is_sp <- inherits(gaz, "SpatialPointsDataFrame")
    if (is_sp) {
        ## if sp, work on the @data object
        gaz_sp <- gaz
        gaz <- gaz@data
    }

    ## features that have a name from one of our countries of interest
    in_ids <- unique(gaz$scar_common_id[gaz$country_name %in% origin_country])
    in_coi <- gaz[gaz$scar_common_id %in% in_ids, ]
    ## order within scar_common_id by origin_country (with ordering as per appearance in the origin_country vector)
    ord <- order(in_coi$scar_common_id, factor(in_coi$country_name, levels=origin_country))
    in_coi <- in_coi[ord, ]
    in_coi <- in_coi[!duplicated(in_coi$scar_common_id), ] ## take first entry for each scar_common_id

    out_coi <- gaz[!gaz$scar_common_id %in% in_ids, ]
    out_coi <- out_coi[!duplicated(out_coi$scar_common_id), ] ## take first entry for each scar_common_id

    out <- rbind(in_coi, out_coi)

    if (is_sp) {
        ## return the subset of gaz_sp corresponding to the rows we just selected
        gaz_sp[gaz_sp$gaz_id %in% out$gaz_id, ]
    } else {
        out
    }
}


#' Thin names to give approximately uniform spatial coverage
#'
#' The provided data.frame of names will be thinned down to a smaller number of names. The thinning process attempts to select a subset of names that are uniformly spatially distributed, while simultaneously choosing the most important names (according to their relative score in the \code{score_col} column.
#'
#' @param gaz data.frame or SpatialPointsDataFrame: as returned by \code{\link{an_read}}
#' @param n numeric: number of names to return
#' @param score_col string: the name of the column that gives the relative score of each name (e.g. as returned by \code{an_suggest}). Names with higher scores will be preferred by the thinning process
#' @param score_weighting numeric: weighting of scores relative to spatial distribution. A lower \code{score_weighting} value will tend to choose lower-scored names
#' in order to achieve better spatial uniformity. A higher \code{score_weighting} value will trade spatial uniformity in favour of selecting
#' higher-scored names
#'
#' @return data.frame
#'
#' @seealso \code{\link{an_read}} \code{\link{an_suggest}}
#'
#' @examples
#' \dontrun{
#'  g <- an_read(cache_directory="c:/temp/gaz")
#'
#'  ## get a single name per feature, preferring the
#'  ##  Australian name where there is one
#'
#'  g <- an_preferred(g, "Australia")
#'
#'  ## suggested names for a 100x100 mm map covering 60-90E, 70-60S
#'  ##  (this is about a 1:12M scale map)
#'
#'  suggested <- an_suggest(g, map_extent = c(60, 90, -70, -60), map_dimensions = c(100, 100))
#'  head(suggested, 20) ## top 20 names by score
#'  an_thin(suggested, 20) ## 20 names chosen for spatial coverage and score
#' }
#'
#' @export
an_thin <- function(gaz, n, score_col = "score", score_weighting = 5){
    ## thin points to give approximately uniform spatial coverage
    ## optionally including scores
    if (n>=nrow(gaz)) return(gaz)
    idx <- rep(FALSE, nrow(gaz))
    ## construct matrix of distances between all pairs of points
    ## note that we use Euclidean distance on coordinates, for computational reasons
    if (inherits(gaz, "SpatialPointsDataFrame")) {
        this.dist <- as.matrix(dist(as.data.frame(gaz)[, c("longitude", "latitude")]))
    } else {
        this.dist <- as.matrix(dist(gaz[, c("longitude", "latitude")]))
    }
    if (!is.null(score_col)) {
        if (inherits(gaz, "SpatialPointsDataFrame")) {
            sc <- as.data.frame(gaz)[, score_col]
        } else {
            sc <- gaz[, score_col]
        }
        if (inherits(sc, "data.frame")) sc <- unlist(sc)
    } else {
        sc <- rep(1, nrow(gaz))
    }
    tmp <- which.max(sc)
    idx[tmp] <- TRUE ## start with the first best-scored points
    sc[tmp] <- NA_real_
    while(sum(idx) < n) {
        ## rank the distances
        ## for each point, find its distance to the closest point that's already been selected
        dist_rank <- rank(apply(this.dist[, idx, drop=FALSE], 1, min), na.last = "keep")
        ## rank the scores
        score_rank <- rank(sc, na.last = "keep")
        ## select the point with highest avg rank (i.e. highest composite distance and score)
        tmp <- which.max(score_weighting * score_rank + dist_rank)
        idx[tmp] <- TRUE
        sc[tmp] <- NA_real_
    }
    gaz[idx, ]
}


#' Suggest names for a map (experimental)
#'
#' Features are given a suitability score based on how often (and at what map scales) they have been named on maps prepared by expert cartographers.
#' This is an experimental function and currently only implemented for \code{map_scale} values of 10 million or larger.
#'
#' @param gaz data.frame or SpatialPointsDataFrame: as returned by \code{\link{an_read}}
#' @param map_scale numeric: the scale of the map (e.g. 20e6 for a 1:20M map). If \code{map_scale} is not provided, it will be estimated from \code{map_extent} and \code{map_dimensions}
#' @param map_extent raster Extent object or vector of c(longitude_min, longitude_max, latitude_min, latitude_max): the extent of the area for which name suggestions are sought. Not required if \code{map_scale} is provided
#' @param map_dimensions numeric: 2-element numeric giving width and height of the map, in mm. Not required if \code{map_scale} is provided
#'
#' @return data.frame of names with a "score" column added. The data.frame will be sorted in descending score order. Names with higher scores are those that are suggested as the most suitable for display.
#'
#' @seealso \code{\link{an_read}} \code{\link{an_thin}}
#'
#' @examples
#' \dontrun{
#'  g <- an_read(cache_directory = "c:/temp/gaz")
#'
#'  ## get a single name per feature, preferring the
#'  ##  Australian name where there is one
#'  g <- an_preferred(g, "Australia")
#'
#'  ## suggested names for a 100x100 mm map covering 60-90E, 70-60S
#'  ##  (this is about a 1:12M scale map)
#'
#'  suggested <- an_suggest(g, map_extent = c(60, 90, -70, -60), map_dimensions = c(100, 100))
#'  head(suggested, 20) ## top 20 names
#' }
#' @export
an_suggest <- function(gaz, map_scale, map_extent, map_dimensions) {
    if (!missing(map_extent))
        assert_that((is.numeric(map_extent) && length(map_extent) == 4) || inherits(map_extent, "Extent"))

    if (missing(map_scale)) {
        if (missing(map_extent) || missing(map_dimensions)) stop("need either map_scale, or map_dimensions and map_extent")
        map_scale <- an_mapscale(map_dimensions, map_extent)
    }
    ## scale >= 10e6: we have full coverage (nearly so for 12mill) of all scar_common_ids, so use per-feature predictions
    ## for scale < 10e6: use predictions by feature properties (except maybe if area of interest lies within a catalogued map)
    ## stations as special case?
    temp <- gaz %>% an_filter(extent=map_extent)
    if (map_scale>=10e6) {
        ## per-feature predictions
        ##load("uidfits.RData") ## in sysdata.rda
        temp$score <- 0
        temp$scale <- map_scale
        idx <- which(temp$scar_common_id %in% uid)
        for (i in idx) {
            fidx <- which(temp$scar_common_id[i] == uid)
            if (inherits(uid_fits[[fidx]], "C5.0")) {
                temp$score[i] <- predict(uid_fits[[fidx]], newdata = temp[i,], type = "prob")[, 2]
            } else {
                temp$score[i] <- uid_fits[[fidx]]
            }
        }
    } else {
        stop("an_suggest not yet implemented for map_scale value below 10 million")
    }
    oidx <- order(temp$score, decreasing = TRUE)
    temp[oidx, ]
}


#' Calculate approximate map scale
#'
#' @param map_dimensions numeric: 2-element numeric giving width and height of the map, in mm
#' @param map_extent raster Extent object or vector of c(longitude_min, longitude_max, latitude_min, latitude_max): the geographic extent of the map
#'
#' @return numeric
#'
#' @examples
#' ## an A3-sized map of the Southern Ocean (1:20M)
#' an_mapscale(c(400, 570), c(-180, 180, -90, -40))
#'
#' @export
an_mapscale <- function(map_dimensions, map_extent) {
    assert_that((is.numeric(map_extent) && length(map_extent) == 4) || inherits(map_extent, "Extent"))
    if (!inherits(map_extent, "Extent")) map_extent <- extent(as.numeric(map_extent))
    mapext <- raster()
    extent(mapext) <- map_extent

    sqrt(cellStats(area(mapext), "sum") * 1e3 * 1e3)/ ## sqrt of (map area in m^2)
        sqrt(prod(map_dimensions) / 1e3 / 1e3) ## sqrt of (map dimension area in m^2)
}


#' Get links to gazetteer entries
#'
#' Each entry in the SCAR Composite Gazetteer of Antarctica has its own web page. The \code{an_url} function will return the URL of the page associated with a given gazetteer entry.
#'
#' @references \url{http://www.scar.org/data-products/cga}
#' @param gaz data.frame or SpatialPointsDataFrame: as returned by \code{\link{an_read}} (most commonly a subset thereof)
#'
#' @return character vector, where each component is a URL to a web page giving more information about the associated gazetteer entry
#'
#' @examples
#' \dontrun{
#'  g <- an_read(cache_directory="c:/temp/gaz")
#'  my_url <- an_url(an_filter(g, "Ufs Island")[1, ])
#'  browseURL(my_url)
#' }
#' @export
an_url <- function(gaz) {
    ## only CGA entries dealt with: needs modification once other gazetteers are added
    out <- rep(NA_character_, nrow(gaz))
    cga_idx <- gaz$gazetteer == "CGA"
    out[cga_idx] <- sprintf("https://data.aad.gov.au/aadc/gaz/scar/display_name.cfm?gaz_id=%d", gaz$gaz_id[cga_idx])
    out
}
