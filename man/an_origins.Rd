% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/search.R
\name{an_origins}
\alias{an_origins}
\title{List the origins of place names present in gazetteer data}
\usage{
an_origins(gaz)
}
\arguments{
\item{gaz}{data.frame or SpatialPointsDataFrame: as returned by \code{\link{an_read}}, \code{\link{an_preferred}}, or \code{\link{an_filter}}}
}
\value{
character vector of origin names (countries or organisations)
}
\description{
The Composite Gazetteer of Antarctica is a compilation of place names provided by different countries and organisations. This function lists the originating bodies that provided the names in a given data frame.
}
\examples{
\dontrun{
 g <- an_read(cache = "session")

 ## which bodies (countries or organisations) provided the names in our data?
 an_origins(g)
}
}
\seealso{
\code{\link{an_filter}} for filtering data according to origin
}
