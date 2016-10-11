library(testthat)
library(dplyr)
library(cga)

g <- load_cga()
test_check("cga")
