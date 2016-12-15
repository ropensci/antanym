#' Find one name per feature according to specified preferences
#'
#' @references \url{http://www.scar.org/data-products/cga}
#' @param gaz data.frame: as returned by \code{\link{an_read}}
#' @param origin_country character: vector of country names. If a given feature has been named by one of these countries, this name will be returned. For valid values see \code{\link{an_countries}}
#'
#' @return data.frame of results
#'
#' @seealso \code{\link{an_read}}
#'
#' @examples
#' \dontrun{
#'  g <- an_read(cache_directory="c:/temp/gaz")
#'
#'  ## get a single name per feature, preferring the
#'  ##  Polish name where there is one
#'  pnames <- an_preferred(g,"Poland")
#'
#'  ## names starting with "Sm", preferring US names then
#'  ##  Australian ones (then whatever is available after that)
#'  g %>% an_filter("^Sm") %>% an_preferred(origin_country=c("United States of America","Australia"))
#' }
#'
#' @export
an_preferred <- function(gaz,origin_country) {
    pn1 <- gaz %>% group_by_("scar_common_id") %>% filter_(~country_name %in% origin_country)
    ## order by origin_country (with ordering as per appearance in the origin_country vector)
    temp <- factor(pn1$country_name,levels=origin_country)
    pn1 <- pn1 %>% arrange_(~scar_common_id,~temp) %>% slice(1L)
    pn2 <- gaz %>% group_by_("scar_common_id") %>% filter_(~!country_name %in% origin_country) %>% slice(1L)
    bind_rows(pn1,pn2 %>% filter_(~!scar_common_id %in% pn1$scar_common_id))
}

## not exported


#' Thin names to give approximately uniform spatial coverage
#'
#' @param gaz data.frame: as returned by \code{\link{an_read}}
#' @param n numeric: number of names to return
#' @param position_cols character: the names of the columns that give the spatial position of the features
#' @param score_col string: the name of the column that gives the relative score (e.g. as returned by \code{an_suggest})
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
#'  g <- an_preferred(g,"Australia")
#'
#'  ## suggested names for a 100x100 mm map covering 60-90E, 70-60S
#'  ##  (this is about a 1:12M scale map)
#'  suggested <- an_suggest(g,map_extent=c(60,90,-70,-60),map_dimensions=c(100,100))
#'  head(suggested,20) ## top 20 names by score
#'  an_thin(suggested,20) ## 20 names chosen for spatial coverage and score
#' }
#'
#' @export
an_thin <- function(gaz,n,position_cols=c("longitude","latitude"),score_col="score",score_weighting=5){
    ## thin points to give approximately uniform spatial coverage
    ## optionally including scores
    if (n>=nrow(gaz)) return(gaz)
    idx <- rep(FALSE,nrow(gaz))
    ##tempij <- expand.grid(1:nrow(gaz),1:nrow(gaz))
    ##this.dist <- distVincentySphere(gaz[tempij[,1],position_cols],gaz[tempij[,2],position_cols])
    this.dist <- as.matrix(dist(gaz[,position_cols]))
    if (!is.null(score_col)) {
        sc <- (gaz[,score_col])
        if (inherits(sc,"data.frame")) sc <- unlist(sc)
    } else {
        sc <- rep(1,nrow(gaz))
    }
    tmp <- which.max(sc)
    idx[tmp] <- TRUE ## start with the first best-scored points
    sc[tmp] <- NA
    while(sum(idx) < n) {
        ## rank the distances
        ## for each point, find its distance to the closest point that's already been selected
        dist_rank <- rank(apply(this.dist[,idx,drop=FALSE],1,min),na.last="keep")
        ## rank the scores
        score_rank <- rank(sc,na.last="keep")
        ## select the point with highest avg rank (i.e. highest composite distance and score)
        tmp <- which.max(score_weighting*score_rank+dist_rank)
        idx[tmp] <- TRUE
        sc[tmp] <- NA
    }
    gaz[idx,]
}


#' Suggest names for a map (experimental)
#'
#' Features are given a suitability score based on how often (and at what map scales) they have been named on maps prepared by expert cartographers.
#' This is an experimental function and currently only implemented for \code{map_scale} values of 20 million or larger, and only for features in the
#' SCAR composite gazetteer (features south of 60S).
#'
#' @param gaz data.frame: as returned by \code{\link{an_read}}
#' @param map_scale numeric: the scale of the map (e.g. 20e6 for a 1:20M map). If \code{map_scale} is not provided, it will be estimated from \code{extent} and \code{map_dimensions}
#' @param map_extent raster Extent object or vector of c(longitude_min,longitude_max,latitude_min,latitude_max): the extent of the area for which name suggestions are sought. Not required if \code{map_scale} is provided
#' @param map_dimensions numeric: 2-element numeric giving width and height of the map, in mm. Not required if \code{map_scale} is provided
#'
#' @return data.frame of names with a "score" column added. The data.frame will be sorted in descending score order. Names with higher scores are those that are suggested as the most suitable for display.
#'
#' @seealso \code{\link{an_read}} \code{\link{an_thin}}
#'
#' @examples
#' \dontrun{
#'  g <- an_read(cache_directory="c:/temp/gaz")
#'
#'  ## get a single name per feature, preferring the
#'  ##  Australian name where there is one
#'  g <- an_preferred(g,"Australia")
#'
#'  ## suggested names for a 100x100 mm map covering 60-90E, 70-60S
#'  ##  (this is about a 1:12M scale map)
#'  suggested <- an_suggest(g,map_extent=c(60,90,-70,-60),map_dimensions=c(100,100))
#'  head(suggested,20) ## top 20 names
#' }
#' @export
an_suggest <- function(gaz,map_scale,map_extent,map_dimensions) {
    if (missing(map_scale)) {
        if (missing(map_extent) || missing(map_dimensions)) stop("need either map_scale, or map_dimensions and map_extent")
        map_scale <- an_mapscale(map_dimensions,map_extent)
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
            fidx <- which(temp$scar_common_id[i]==uid)
            if (inherits(uid_fits[[fidx]],"C5.0")) {
                temp$score[i] <- predict(uid_fits[[fidx]],newdata=temp[i,],type="prob")[,2]
            } else {
                temp$score[i] <- uid_fits[[fidx]]
            }
        }
    } else {
        stop("an_suggest not yet implemented for map_scale value below 10 million")
    }
    temp %>% arrange_(~desc(score))
    #an_thin(temp,n)
}
# @param preferred_types character: a vector of preferred feature type names (in order of preference). The suggestion algorithm will try to favour these feature types over others [not yet implemented]


#' Calculate approximate map scale
#'
#' @param map_dimensions numeric: 2-element numeric giving width and height of the map, in mm
#' @param map_extent raster Extent object or vector of c(longitude_min,longitude_max,latitude_min,latitude_max): the geographic extent of the map
#'
#' @return numeric
#'
#' @examples
#' ## an A3-sized map of the Southern Ocean (1:20M)
#' an_mapscale(c(400,570),c(-180,180,-90,-40))
#'
#' @export
an_mapscale <- function(map_dimensions,map_extent) {
    if (!inherits(map_extent,"Extent")) map_extent <- extent(as.numeric(map_extent))
    mapext <- raster()
    extent(mapext) <- map_extent

    sqrt(cellStats(area(mapext),"sum")*1e3*1e3)/ ## sqrt of (map area in m^2)
        sqrt(prod(map_dimensions)/1e3/1e3) ## sqrt of (map dimension area in m^2)
}


#' Get links to gazetteer entries
#'
#' @references \url{http://www.scar.org/data-products/cga}
#' @param gaz data.frame: as returned by \code{\link{an_read}} (most commonly a subset thereof)
#'
#' @return character vector, where each component is a URL to a web page giving more information about the associated gazetteer entry
#'
#' @examples
#' \dontrun{
#'  g <- an_read(cache_directory="c:/temp/gaz")
#'  my_url <- an_url(an_filter(g,"Ufs Island")[1,])
#'  browseURL(my_url)
#' }
#' @export
an_url <- function(gaz) {
    ## only CGA entries dealt with: needs modification once other gazetteers are added
    out <- rep(as.character(NA),nrow(gaz))
    cga_idx <- gaz$gazetteer=="cga"
    out[cga_idx] <- sprintf("https://data.aad.gov.au/aadc/gaz/scar/display_name.cfm?gaz_id=%d",gaz$gaz_id[cga_idx])
    out
}
