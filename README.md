
<!-- README.md is generated from README.Rmd. Please edit that file -->
antanym
=======

[![Travis-CI Build Status](https://travis-ci.org/SCAR/antanym.svg?branch=master)](https://travis-ci.org/SCAR/antanym) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/SCAR/antanym?branch=master&svg=true)](https://ci.appveyor.com/project/SCAR/antanym) [![codecov](https://codecov.io/gh/SCAR/antanym/branch/master/graph/badge.svg)](https://codecov.io/gh/SCAR/antanym)

Overview
--------

This R package provides easy access to Antarctic geographic place name information, and tools for working with those names.

The authoritative source of place names in Antarctica is the SCAR Composite Gazetteer of Antarctica (CGA), which consists of approximately 37,000 names corresponding to 19,000 distinct features. It covers features south of 60 °S, including terrestrial and undersea or under-ice.

There is no single naming authority responsible for place names in Antarctica because it does not fall under the sovereignty of any one nation. In general, individual countries have administrative bodies that are responsible for their national policy on, and authorisation and use of, Antarctic names. The CGA is a compilation of place names that have been submitted by representatives of national names committees from 22 countries.

The composite nature of the CGA means that there may be multiple names associated with a given feature. Consider using the `an_preferred()` function for resolving a single name per feature.

For more information, see the [CGA home page](http://data.aad.gov.au/aadc/gaz/scar/). The CGA was begun in 1992. Since 2008, Italy and Australia have jointly managed the CGA, the former taking care of the editing, the latter maintaining the database and website. The SCAR [Standing Committee on Antarctic Geographic Information (SCAGI)](http://www.scar.org/data-products/scagi) coordinates the project. This R package is a product of the SCAR [Expert Group on Antarctic Biodiversity Informatics](http://www.scar.org/ssg/life-sciences/eg-abi) and SCAGI.

### Citing

The SCAR Composite Gazetteer of Antarctica is made available under a CC-BY license. If you use it, please cite it:

> Composite Gazetteer of Antarctica, Scientific Committee on Antarctic Research. GCMD Metadata (<http://gcmd.nasa.gov/records/SCAR_Gazetteer.html>)

Installing
----------

``` r
install.packages("devtools")
devtools::install_github("SCAR/antanym")
```

Usage
-----

Start by fetching the names data from its host server. Here we cache it to a temporary directory so that we can access it later in the session without needing to re-download it:

``` r
library(antanym)
cache_dir <- tempdir()
g <- an_read(cache_dir = cache_dir)
```

If you prefer working with `sp` objects, then this will return a `SpatialPointsDataFrame`:

``` r
gsp <- an_read(sp = TRUE)
```

How many names do we have in total?

``` r
nrow(g)
#> [1] 37629
```

Corresponding to how many distinct features?

``` r
length(unique(g$scar_common_id))
#> [1] 19569
```

Choose one name per feature, preferring the Polish name where there is one:

``` r
g <- an_preferred(g, origin_country = "Poland")
nrow(g)
#> [1] 19569
```

Find islands within 20km of 100E, 66S:

``` r
nms <- an_near(an_filter(g, feature_type = "Island"), c(100, -66), 20)

## or equivalently, using the pipe operator
nms <- g %>% an_filter(feature_type = "Island") %>% an_near(c(100, -66), 20)

nms[,c("place_name", "longitude", "latitude")]
#> # A tibble: 1 x 3
#>   place_name    longitude latitude
#>   <chr>             <dbl>    <dbl>
#> 1 Foster Island       100    -66.1
```

Find names starting with "Slom":

``` r
an_filter(g, "^Slom")[,c("place_name", "longitude", "latitude")]
#> # A tibble: 2 x 3
#>   place_name     longitude latitude
#>   <chr>              <dbl>    <dbl>
#> 1 Sloman Glacier     -68.6    -67.7
#> 2 Slomer Cove        -59.4    -63.8
```

### Name suggestions

Antanym includes an experimental function that will suggest which features might be best to add names to on a given map. These suggestions are based on maps prepared by expert cartographers, and the features that were explicitly named on those maps.

Let's say we are preparing a figure of the greater Prydz Bay region (60–90 °E, 65–70 °S), to be shown at 80mm x 80mm in size (this is approximately a 1:10M scale map). We can ask for suggested names to show on this map:

``` r
my_longitude <- c(60, 90)
my_latitude <- c(-70, -65)
suggested <- an_suggest(g, map_extent = c(my_longitude, my_latitude), map_dimensions = c(80, 80))
```

Plot the top ten names purely by score:

``` r
this_names <- head(suggested, 10)

library(rworldmap)
map <- getMap(resolution = "low")
plot(map, xlim = my_longitude, ylim = my_latitude)
points(this_names$longitude, this_names$latitude, col = "blue")
pos <- rep(c(1, 2, 3, 4), ceiling(nrow(this_names)/4)) ## alternate positions of labels to reduce overlap
pos[order(this_names$longitude)] <- pos[1:nrow(this_names)]
text(this_names$longitude, this_names$latitude, labels = this_names$place_name, pos = pos)
```

<img src="vignettes/README-unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

Or the ten best names considering both score and spatial coverage:

``` r
this_names <- an_thin(suggested, 10)
plot(map, xlim = my_longitude, ylim = my_latitude)
points(this_names$longitude, this_names$latitude, col = "blue")
pos <- rep(c(1, 2, 3, 4), ceiling(nrow(this_names)/4)) ## alternate positions of labels to reduce overlap
pos[order(this_names$longitude)] <- pos[1:nrow(this_names)]
text(this_names$longitude, this_names$latitude, labels = this_names$place_name, pos = pos)
```

<img src="vignettes/README-unnamed-chunk-11-1.png" style="display: block; margin: auto;" />

Other map examples
------------------

A [leaflet app](https://australianantarcticdatacentre.github.io/antanym-demo/leaflet.html) using Mercator projection and clustered markers for place names.

<a href="https://australianantarcticdatacentre.github.io/antanym-demo/leaflet.html"><img src="vignettes/README-leaflet.png" width="40%" /></a>

And a similar example using a [polar stereographic projection](https://australianantarcticdatacentre.github.io/antanym-demo/leafletps.html).

<a href="https://australianantarcticdatacentre.github.io/antanym-demo/leafletps.html"><img src="vignettes/README-leafletps.png" width="40%" /></a>

See the [antanym-demo](https://github.com/AustralianAntarcticDataCentre/antanym-demo) repository for the source code of these examples.

Future directions
-----------------

Antanym currently only provides information from the SCAR CGA. This does not cover other features that may be of interest to Antarctic researchers, such as those on subantarctic islands or features that have informal names not registered in the CGA. Antanym may be expanded to cover extra gazetters containing such information, at a later date.
