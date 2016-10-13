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
