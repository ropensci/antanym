#' Find one name per feature in the SCAR Composite Gazetteer
#'
#' The SCAR Composite Gazetteer of Antarctic is a compilation of place names provided by different countries and organisations. The composite nature of the CGA means that there may be multiple names associated with a single feature. The \code{an_preferred} function can be used to resolve a single name per feature. Provide one or more \code{origin} entries and the input \code{gaz} will be filtered to a single name per feature. For features that have multiple names (e.g. have been named by multiple countries) a single name will be chosen, preferring names from the specified \code{origin} bodies where possible.
#'
#' @references \url{http://www.scar.org/data-products/cga}
#' @param gaz data.frame or SpatialPointsDataFrame: as returned by \code{\link{an_read}}
#' @param origin character: vector of preferred name origins (countries or organisations), in order of preference. If a given feature has been named by one of these bodies, this place name will be chosen. If the feature in question has not been given a name by any of these bodies, a place name given by another body will be chosen, with preference according to the \code{unmatched} parameter. For valid \code{origin} values, see \code{\link{an_origins}}
#' @param unmatched string: how should names be chosen for features that have not been been named by one of the preferred \code{origin} bodies? Valid values are "random" (the non-preferred originating bodies will be randomly ordered) or "count" (the non-preferred originating bodies will be ordered by their number of entries, with the largest first)
#'
#' @return data.frame of results
#'
#' @seealso \code{\link{an_read}}, \code{\link{an_origins}}
#'
#' @examples
#' \dontrun{
#'  g <- an_read(cache = "session")
#'
#'  ## get a single name per feature, preferring the
#'  ##  Polish name where there is one
#'  pnames <- an_preferred(g, origin = "Poland")
#'
#'  ## names starting with "Sm", preferring US names then
#'  ##  Australian ones if available
#'  g %>% an_filter("^Sm") %>%
#'        an_preferred(origin = c("United States of America", "Australia"))
#' }
#'
#' @export
an_preferred <- function(gaz, origin, unmatched = "random") {
    assert_that(inherits(gaz, c("data.frame", "SpatialPointsDataFrame")))
    assert_that(is.string(unmatched))
    unmatched <- match.arg(tolower(unmatched), c("random", "count"))
    assert_that(is.character(origin))
    is_sp <- inherits(gaz, "SpatialPointsDataFrame")
    if (is_sp) {
        ## if sp, work on the @data object
        gaz_sp <- gaz
        gaz <- gaz@data
    }
    ## features that have a name from one of our sources of interest
    ## determine the order of preference of sources that aren't in our preferred list
    not_pref_origins <- names(sort(table(gaz$origin), decreasing = TRUE))
    ## those are in decreasing order of count (i.e. unmatched = "count")
    if (unmatched == "random") not_pref_origins <- sample(not_pref_origins, size = length(not_pref_origins), replace = FALSE)
    ## tack these onto the tail end of our preferred list
    origin <- c(origin, setdiff(not_pref_origins, origin))
    in_ids <- unique(gaz$scar_common_id[gaz$origin %in% origin])
    in_coi <- gaz[gaz$scar_common_id %in% in_ids, ]

    ## order scar_common_id by origin
    ## (with ordering as per appearance in the origin vector)
    ord <- order(in_coi$scar_common_id, factor(in_coi$origin, levels = origin))
    in_coi <- in_coi[ord, ]
    in_coi <- in_coi[!duplicated(in_coi$scar_common_id), ] ## take first entry for each scar_common_id
    ## now add any features that did not have an entry from our preferred origins
    ## this should not ever happen, but do it here just in case e.g. there are NA entries in origin
    not_in_coi <- gaz[!gaz$scar_common_id %in% in_ids, ]
    not_in_coi <- not_in_coi[!duplicated(not_in_coi$scar_common_id), ] ## take first entry for each scar_common_id
    out <- rbind(in_coi, not_in_coi)
    if (is_sp) {
        ## return the subset of gaz_sp corresponding to the rows we just selected
        gaz_sp[gaz_sp$gaz_id %in% out$gaz_id, ]
    } else {
        out
    }
}
