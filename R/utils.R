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



#' Suggest names for a map (experimental)
#'
#'
#'
#' @references \url{http://www.scar.org/data-products/cga}
#' @param gaz data.frame: as returned by \code{\link{an_read}}
#' @param scale numeric: the scale of the map (e.g. 20e6 for a 1:20M map). If \code{scale} is not provided, it will be estimated from \coce{extent} and \code{map_dimensions}
#' @param map_extent raster Extent object or vector of c(longitude_min,longitude_max,latitude_min,latitude_max): the extent of the area for which name suggestions are sought. Not required if \code{scale} is provided
#' @param map_dimensions numeric: 2-element numeric giving width and height of the map, in mm. Not required if \code{scale} is provided
#' @param preferred_types character: a vector of preferred feature type names (in order of preference). The suggestion algorithm will try to favour these feature types over others
#'
#' @return data.frame
#'
#' @seealso \code{\link{an_read}}
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
#'  suggested <- an_suggest(g,c(60,90,-70,-60),c(100,100))
#' }
#' @export
an_suggest <- function(gaz,scale,map_extent,map_dimensions,preferred_types) {
    if (missing(scale)) {
        if (missing(map_extent) || missing(map_dimensions)) stop("need either scale, or map_dimensions and map_extent")
        scale <- an_mapscale(map_dimensions,map_extent)
    }
    ## first cut by spatial map_extent
    gaz <- filter_(gaz,~longitude>=map_extent[1] & longitude<=map_extent[2] & latitude>=map_extent[3] & latitude<=map_extent[4])
    ## spatial density of features
    ##ll_grid <- expand.grid(seq(map_extent[1],map_extent[2],length.out=20),seq(map_extent[3],map_extent[4],length.out=20))
    #dens <- kde2d(gaz$longitude,gaz$latitude,n=20,lims=map_extent)
}


#' Calculate map scale
#'
#' @param map_dimensions numeric: 2-element numeric giving width and height of the map, in mm
#' @param map_extent raster Extent object or vector of c(longitude_min,longitude_max,latitude_min,latitude_max): the extent of the area for which name suggestions are sought
#'
#' @return numeric
#'
#' @examples
#' ## an A3-sized map of the Southern Ocean (1:20M)
#' an_mapscale(c(400,570),c(-180,180,-90,-40))
#'
#' @export
an_mapscale <- function(map_dimensions,map_extent) {
    mapext <- raster()
    if (inherits(map_extent,"Extent"))
        extent(mapext) <- map_extent
    else
        extent(mapext) <- extent(as.numeric(map_extent))
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
