.onAttach <- function(libname, pkgname) {
    packageStartupMessage(
        paste("The Composite Gazetteer of Antarctica is made available under a CC-BY license.",
              "Please cite it:\nSecretariat SCAR (1992 updated", format(Sys.Date(), "%Y)"),
              "Composite Gazetteer of Antarctica, Scientific Committee on Antarctic Research. GCMD Metadata (http://gcmd.nasa.gov/records/SCAR_Gazetteer.html)",
              "Data obtained via the antanym R package: https://github.com/ropensci/antanym"))
}
