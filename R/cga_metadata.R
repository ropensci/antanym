#' Information about the Composite Gazetteer of Antarctica data structure
#'
#' The Composite Gazetteer of Antarctica data structure (as returned by \code{\link{an_read}}):
#'
#' @references \url{https://data.aad.gov.au/aadc/gaz/scar/}, \url{https://www.scar.org/data-products/place-names/}
#'
#' @param simplified logical: if TRUE, only describe the simplified set of columns (see the equivalent parameter in \code{\link{an_read}})
#'
#' @return a data frame with columns "field" and "description"
#'
#' @seealso \code{\link{an_read}}
#'
#' @examples
#'
#' an_cga_metadata()
#'
#' @export

an_cga_metadata <- function(simplified = TRUE) {
    assert_that(is.flag(simplified), !is.na(simplified))


    md <- rbind(list(f = "gaz_id", desc = "The unique identifier of each gazetteer entry. Note that the same feature (e.g. \'Browns Glacier\') might have multiple gazetteer entries, each with their own gaz_id, because the feature has been named multiple times by different naming authorities. The scar_common_id value for these entries will be identical, because scar_common_id identifies the feature itself", simp = TRUE),
                list(f = "scar_common_id", desc = "The unique identifier (in the Composite Gazetteer of Antarctica) of the feature. A single feature may have multiple names, given by different naming authorities", simp = TRUE),
                list(f = "place_name", desc = "The name of the feature", simp = TRUE),
                list(f = "place_name_transliterated", desc = "The name of the feature transliterated to simple ASCII characters (e.g. with diacritical marks removed)", simp = TRUE),
                list(f = "longitude", desc = "The longitude of the feature (negative values indicate degrees west). Note that many features are not point features (e.g. mountains, lakes), in which case the longitude and latitude values are indicative only, generally of the centroid of the feature", simp = TRUE),
                list(f = "latitude", desc = "The latitude of the feature (negative values indicate degrees south). Note that many features are not point features (e.g. mountains, lakes), in which case the longitude and latitude values are indicative only, generally of the centroid of the feature", simp = TRUE),
                list(f = "altitude", desc = "The altitude of the feature, in metres relative to sea level. Negative values indicate features below sea level", simp = TRUE),
                list(f = "feature_type_name", desc = "The feature type (e.g. \'Archipelago\', \'Channel\', \'Mountain\'). See an_feature_types for a full list", simp = TRUE),
                list(f = "date_named", desc = "The date on which the feature was named", simp = TRUE),
                list(f = "narrative", desc = "A text description of the feature; may include a synopsis of the history of its name", simp = TRUE),
                list(f = "named_for", desc = "The person after whom the feature was named, or other reason for its naming. For historical reasons the distinction between 'narrative' and 'named for' is not always obvious", simp = TRUE),
                list(f = "origin", desc = "The naming authority that provided the name. This is a country name, or organisation name for names that did not come from a national source", simp = TRUE),
                list(f = "relic", desc = "If TRUE, this name is associated with a feature that no longer exists (e.g. an ice shelf feature that has disappeared)", simp = TRUE),
                list(f = "gazetteer", desc = "The gazetteer from which this information came (currently only 'CGA')", simp = TRUE),
                list(f = "meeting_date", desc = "The date on which the name was formally approved by the associated naming authority. This is not available for many names: see the date_named column", simp = FALSE),
                list(f = "meeting_paper", desc = "References to papers or documents associated with the naming of the feature", simp = FALSE),
                list(f = "remote_sensor_info", desc = "Text describing the remote sensing information (e.g. satellite platform name and image details) used to define the feature, if applicable", simp = FALSE),
                list(f = "coordinate_accuracy", desc = "An indicator of the accuracy of the coordinates, in metres", simp = FALSE),
                list(f = "altitude_accuracy", desc = "An indicator of the accuracy of the altitude value, in metres", simp = FALSE),
                list(f = "cga_source_gazetteer", desc = "For the Composite Gazetteer, this entry gives the source gazetteer from which this entry was taken. This is currently either a three-letter country code (e.g. 'ESP', 'USA') or 'GEBCO' (for the GEBCO gazetteer of undersea features)", simp = FALSE),
                list(f = "country_name", desc = "The full name of the country where cga_source_gazetteer is a country", simp = FALSE),
                list(f = "source_name", desc = "The cartographic/GIS/remote sensing source from which the coordinates were derived", simp = FALSE),
                list(f = "source_publisher", desc = "Where coordinates were derived from a map, the publisher of that map", simp = FALSE),
                list(f = "source_scale", desc = "The scale of the map from which the coordinates were derived", simp = FALSE),
                list(f = "source_institution", desc = "The institution from which the coordinate information came", simp = FALSE),
                list(f = "source_person", desc = "The contact person at the source institution, if applicable", simp = FALSE),
                list(f = "source_country_code", desc = "The country from which the coordinate information came", simp = FALSE),
                list(f = "source_identifier", desc = "Where a coordinate or elevation was derived from a map, the identifier of that map", simp = FALSE),
                list(f = "comments", desc = "Comments about the name or naming process", simp = FALSE))
    md <-  setNames(as.data.frame(md, stringsAsFactors = FALSE),
                    c("field", "description", "simplified"))
    if (simplified) md <- md[md$simplified == "TRUE", ]
    md[, c("field", "description")]
}

