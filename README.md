[![Travis-CI Build Status](https://travis-ci.org/AustralianAntarcticDataCentre/agn.svg?branch=master)](https://travis-ci.org/AustralianAntarcticDataCentre/agn)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/AustralianAntarcticDataCentre/agn?branch=master&svg=true)](https://ci.appveyor.com/project/AustralianAntarcticDataCentre/agn)
# agn
This R package provides easy access to Antarctic geographic place name information, including the Composite Gazetteer of Antarctica.

The SCAR Composite Gazetteer of Antarctica (CGA), as the name suggests, is a composite or collection of all those names of features that have been submitted by representatives of national gazetteers. It includes the names of features south of 60° S, both terrestrial and undersea or under-ice. The CGA is a compilation of recognized features, with a numerical Unique Identifier code (UID) assigned to each of them, jointly with a list of applicable place names. Since 2008, Italy and Australia jointly have managed the CGA, the former taking care of the editing, the latter maintaining database and website. The SCAR Standing Committee on Antarctic Geographic Information (SCAGI) coordinates the project.

Note that the CGA is a *composite* gazetteer, and so may contain more than one name for a given feature (place). Consider using the `agn_preferred()` function for resolving a single name per feature.


Installing
----------

``` r
install.packages("devtools")
library(devtools)
install_github("AustralianAntarcticDataCentre/agn")
```

Usage
-----

``` r
library(agn)
library(dplyr)
g <- agn_read()

## islands near 100E, 66S
g %>% agn_near(c(100,-66),20) %>% agn_filter(feature_type="Island")

## one name per feature
## names starting with "Sm", preferring the Polish name where there is one
g %>% agn_filter("^Sm") %>% agn_preferred(origin_country=c("Poland"))
```


Demos
-----

- [Simple leaflet app](https://australianantarcticdatacentre.github.io/agn-demo/leaflet.html)
