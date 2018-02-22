library(readr)
library(C5.0)

thisx <- read_csv("data-raw/map_feature_data.csv")
## this data has been compiled from a number of maps prepared by expert
## cartographers
## it shows which features could have been named on a given map (i.e.
## were within the bounds of the map) and whether each one was actually
## named or not

## goal: to be able to predict which features should be named on a given map

## for small scale maps (scale >= 10M): we have full coverage (or nearly so for 12mill) of all scar_common_ids
## that is, we have maps that cover the full list of named features and we
## know whether or not each feature was shown on a given expert map
## for these we can just have a model for each feature
## here we use a C5.0 model that takes the map scale as the sole predictor
## of whether the feature should be shown or not
## for features that are always shown (or never shown) we don't bother building a model
## just default to TRUE or FALSE as appropriate

thisx <- thisx[thisx$scale>=10e6,c("scar_common_id","place_name","named","scale")]
uid <- unique(thisx$scar_common_id)
uid_fits <- lapply(uid,function(z) {
    temp <- subset(thisx,scar_common_id==z)
    if (length(unique(temp$named))>1)
        C5.0(as.factor(named)~scale,data=temp)
    else {
        if (isTRUE(unique(temp$named))) {
            1
        } else {
            0
        }
    }
})
devtools::use_data(uid,uid_fits,internal=TRUE,overwrite=TRUE)
