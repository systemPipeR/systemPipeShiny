library(testthat)
library(systemPipeShiny)
test_check('systemPipeShiny')
identical(Sys.getenv("NOT_CRAN"), "true")
