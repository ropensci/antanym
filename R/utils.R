#' Find one name per feature according to specified preferences
#'
#' @references \url{http://www.scar.org/data-products/cga}
#' @param gaz data.frame: as returned by \code{\link{agn_read}}
#' @param origin_country character: vector of country names. If a given feature has been named by one of these countries, this name will be returned. For valid values see \code{\link{agn_countries}}
#'
#' @return data.frame of results
#'
#' @seealso \code{\link{agn_read}}
#'
#' @examples
#' \dontrun{
#'  g <- agn_read(cache_directory="c:/temp/gaz")
#'
#'  ## get a single name per feature, preferring the
#'  ##  Polish name where there is one
#'  pnames <- agn_preferred(g,"Poland")
#'
#'  ## names starting with "Sm", preferring US names then
#'  ##  Australian ones (then whatever is available after that)
#'  g %>% agn_filter("^Sm") %>% agn_preferred(origin_country=c("United States of America","Australia"))
#' }
#'
#' @export
agn_preferred <- function(gaz,origin_country) {
    pn1 <- gaz %>% group_by_("scar_common_id") %>% filter_(~country_name %in% origin_country)
    ## order by origin_country (with ordering as per appearance in the origin_country vector)
    temp <- factor(pn1$country_name,levels=origin_country)
    pn1 <- pn1 %>% arrange_(~scar_common_id,~temp) %>% slice(1L)
    pn2 <- gaz %>% group_by_("scar_common_id") %>% filter_(~!country_name %in% origin_country) %>% slice(1L)
    bind_rows(pn1,pn2 %>% filter_(~!scar_common_id %in% pn1$scar_common_id))
}

