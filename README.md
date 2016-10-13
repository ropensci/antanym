[![Travis-CI Build Status](https://travis-ci.org/AustralianAntarcticDataCentre/antanym.svg?branch=master)](https://travis-ci.org/AustralianAntarcticDataCentre/antanym)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/AustralianAntarcticDataCentre/antanym?branch=master&svg=true)](https://ci.appveyor.com/project/AustralianAntarcticDataCentre/antanym)
# antanym
This R package provides easy access to Antarctic geographic place name information, including the Composite Gazetteer of Antarctica.

The SCAR Composite Gazetteer of Antarctica (CGA), as the name suggests, is a composite or collection of all those names of features that have been submitted by representatives of national gazetteers. It includes the names of features south of 60° S, both terrestrial and undersea or under-ice. The CGA is a compilation of recognized features, with a numerical Unique Identifier code (UID) assigned to each of them, jointly with a list of applicable place names. Since 2008, Italy and Australia jointly have managed the CGA, the former taking care of the editing, the latter maintaining database and website. The SCAR Standing Committee on Antarctic Geographic Information (SCAGI) coordinates the project.

Note that the CGA is a *composite* gazetteer, and so often contains more than one name for a given feature. Consider using the `an_preferred()` function for resolving a single name per feature.

References
----------

Composite Gazetteer of Antarctica, Scientific Committee on Antarctic Research. GCMD Metadata (http://gcmd.nasa.gov/records/SCAR_Gazetteer.html)



Installing
----------

``` r
install.packages("devtools")
library(devtools)
install_github("AustralianAntarcticDataCentre/antanym")
```

Usage
-----

``` r
library(antanym)
library(dplyr)
g <- an_read()

## islands within 20km of 100E, 66S
g %>% an_near(c(100,-66),20) %>% an_filter(feature_type="Island")

## one name per feature
## names starting with "Sm", preferring the Polish name where there is one
g %>% an_filter("^Sm") %>% an_preferred(origin_country="Poland")
```


Demos
-----

### [Simple leaflet app](https://australianantarcticdatacentre.github.io/antanym-demo/leaflet.html)

Source code:

``` r
library(antanym)
library(dplyr)
library(leaflet)
g <- an_read()

## find single name per feature, preferring United Kingdom names where available
temp <- g %>% an_preferred("United Kingdom")

## only rows with valid locations
temp <- temp[!is.na(temp$longitude) & !is.na(temp$latitude),]

## replace NAs with empty strings in narrative
temp$narrative[is.na(temp$narrative)] <- ""

## formatted popup HTML
popup <- sprintf("<h1>%s</h1><p><strong>Country of origin:</strong> %s<br /><strong>Longitude:</strong> %g<br /><strong>Latitude:</strong> %g<br /><a href=\"https://data.aad.gov.au/aadc/gaz/scar/display_name.cfm?gaz_id=%d\">Link to SCAR gazetteer</a></p>",temp$place_name,temp$country_name,temp$longitude,temp$latitude,temp$gaz_id)

m <- leaflet() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addMarkers(lng=temp$longitude,lat=temp$latitude,group="placenames",
    clusterOptions = markerClusterOptions(),popup=popup,
    label=temp$place_name,labelOptions=labelOptions(textOnly=TRUE))
```
