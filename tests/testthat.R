library(testthat)
library(dplyr)
library(agn)

g <- agn_read()
test_check("agn")
