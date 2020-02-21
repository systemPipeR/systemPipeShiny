
# systemPipeS

<!-- badges: start -->
<!-- badges: end -->

An online demo of [SystempipeS](https://lezhang.shinyapps.io/systemPipeS/). This application is hosted by a small server. Please do not use it for production activities. Heavy taks will crash it and disconnect you from it. 

## Installation

You can install the released version of systemPipeS from Github with:

``` r
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
BiocManager::install("systemPipeR/systemPipeS", build_vignettes=TRUE, dependencies=TRUE)
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(systemPipeS)
## basic example code
## To be added...
```

## Internal 

Run to update the website (./docs/ folder)

``` r
pkgdown::build_site()
```
