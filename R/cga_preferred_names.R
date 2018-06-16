#' Find one name per feature in the SCAR Composite Gazetteer
#'
#' The SCAR Composite Gazetteer of Antarctic is a compilation of place names provided by many different countries. The composite nature of the CGA means that there may be multiple names associated with a single feature. The \code{an_preferred} function can be used to resolve a single name per feature. Specify one or more \code{cga_source} entries and the input \code{gaz} will be filtered to a single name per feature. For features that have multiple names (i.e. appear in multiple source gazetteers) a single name will be chosen, preferring names from the specified \code{cga_source} entries where possible.
#'
#' @references \url{http://www.scar.org/data-products/cga}
#' @param gaz data.frame or SpatialPointsDataFrame: as returned by \code{\link{an_read}}
#' @param origin_country character: deprecated. Use \code{cga_source} instead
#' @param cga_source character: vector of source gazetteers, in order of preference. If a given feature has been named in one of these gazetteers, this place name will be chosen. If the feature in question has not been given a name in any of these gazetteers, a place name given in another gazetteer will be chosen, with preference according to the \code{unmatched} parameter. For valid \code{cga_source} values, see \code{\link{an_cga_sources}}
#' @param unmatched string: how should names be chosen for features that do not have a name in one of the preferred \code{cga_source} gazetteers? Valid values are "random" (the non-preferred gazetteers will be randomly ordered) or "count" (the non-preferred gazetteers will be ordered by their number of entries, with the largest gazetteer first)
#'
#' @return data.frame of results
#'
#' @seealso \code{\link{an_read}}, \code{\link{an_cga_sources}}
#'
#' @examples
#' \dontrun{
#'  g <- an_read(cache = "session")
#'
#'  ## get a single name per feature, preferring the
#'  ##  Polish name where there is one
#'  pnames <- an_preferred(g, cga_source = "POL")
#'
#'  ## names starting with "Sm", preferring US names then
#'  ##  Australian ones if available
#'  g %>% an_filter("^Sm") %>% an_preferred(cga_source = c("USA", "AUS"))
#' }
#'
#' @export
an_preferred <- function(gaz, origin_country, cga_source, unmatched="random") {
    assert_that(inherits(gaz, c("data.frame", "SpatialPointsDataFrame")))
    assert_that(is.string(unmatched))
    unmatched <- match.arg(tolower(unmatched), c("random", "count"))
    if (!missing(origin_country)) {
        warning("the 'origin_country' parameter is deprecated and will be removed in a future release. Use 'cga_source' instead")
        assert_that(is.character(origin_country))
    }
    if (!missing(cga_source)) assert_that(is.character(cga_source))
    is_sp <- inherits(gaz, "SpatialPointsDataFrame")
    if (is_sp) {
        ## if sp, work on the @data object
        gaz_sp <- gaz
        gaz <- gaz@data
    }
    use_cga_source <- !missing(cga_source) ## if FALSE, use origin_country
    ## features that have a name from one of our gazetteers/countries of interest
    if (use_cga_source) {
        ## determine the order of preference of gazetteers that aren't in our preferred list
        out_pref <- names(sort(table(gaz$cga_source_gazetteer), decreasing=TRUE))
        ## those are in decreasing order of count (i.e. unmatched="count")
        if (unmatched=="random") out_pref <- sample(out_pref, length(out_pref))
        ## tack these onto the tail end of our preferred list
        cga_source <- c(cga_source, setdiff(out_pref, cga_source))
        in_ids <- unique(gaz$scar_common_id[gaz$cga_source_gazetteer %in% cga_source])
    } else {
        out_pref <- names(sort(table(gaz$country_name), decreasing=TRUE))
        if (unmatched=="random") out_pref <- sample(out_pref, length(out_pref))
        origin_country <- c(origin_country, setdiff(out_pref, origin_country))
        in_ids <- unique(gaz$scar_common_id[gaz$country_name %in% origin_country])
    }
    in_coi <- gaz[gaz$scar_common_id %in% in_ids, ]

    ## order scar_common_id by cga_source_gazetteer/origin_country
    ## (with ordering as per appearance in the cga_source/origin_country vector)
    if (use_cga_source) {
        ord <- order(in_coi$scar_common_id, factor(in_coi$cga_source_gazetteer, levels=cga_source))
    } else {
        ord <- order(in_coi$scar_common_id, factor(in_coi$country_name, levels=origin_country))
    }
    in_coi <- in_coi[ord, ]
    in_coi <- in_coi[!duplicated(in_coi$scar_common_id), ] ## take first entry for each scar_common_id
    ## now add any features that did not have an entry from our preferred gazetteers/countries
    ## this should not ever happen, but may do if e.g. there are NA entries in cga_source_gazetteer
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
