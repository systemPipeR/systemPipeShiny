
# systemPipeShiny <img src="https://github.com/systemPipeR/systemPipeShiny-book/blob/master/img/sps.png?raw=true" align="right" height="139" />

<!-- badges: start -->
![R-CMD-check](https://github.com/systemPipeR/systemPipeShiny/workflows/R-CMD-check/badge.svg)
<!-- badges: end -->

`systemPipeShiny`(SPS) a Shiny-based R/Bioconductor package that extends the widely used 
[systemPipeR](http://www.bioconductor.org/packages/release/bioc/html/systemPipeR.html) workflow environment and data visualization with a versatile graphical user interface.

There is an online demo of [systempipeShiny](https://tgirke.shinyapps.io/systemPipeShiny/). 
This application is hosted by a small server. Do not use it for production activities. 
Heavy tasks will crash it and disconnect you from it. 

## Installation

To install the package, please use the `BiocManager::install` command:

``` r
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
BiocManager::install("systemPipeShiny", build_vignettes=TRUE, dependencies=TRUE)

```
To obtain the most recent updates immediately, one can install it directly from 
[GitHub](https://github.com/systemPipeR/systemPipeShiny) as follow:

``` r
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
BiocManager::install("systemPipeR/systemPipeShiny", build_vignettes=TRUE, dependencies=TRUE)
```

If you are on Linux, you may also need the following libraries. Different distributions 
may have different commands, but the following commands are examples for Ubuntu:

```
sudo apt-get install libcurl4-openssl-dev
sudo apt-get install libv8-dev
sudo apt-get install libxm12-dev
sudo apt-get install libssl-dev
```

## Quick start

This is a basic example which shows how to use `systempipeShiny` package:

``` r
## Imports the library
library(systemPipeShiny)
## Creates the project directory
spsInit()
```

By default, a project folder is created and named as `SPS_`+`DATE`. 
This project folder provides all the necessary files to launch the application. If you are using Rstudio, `global.R` file will be opened automatically and this is the only file you may need to make custom changes if there is any.

``` r
## Launching the interface
shiny::runApp()
```

# Contact

For additional details regarding the functions of `systempipeShiny`, please consult 
the vignette available [here](https://systempipe.org/systemPipeShiny/articles/systemPipeShiny.html).
 
Please use https://github.com/systemPipeR/systemPipeShiny/issues for reporting bugs, 
issues or for suggesting new features to be implemented.

## Internal 

<details>
<summary><b>
Click to expand the list of internal notes.
</b></summary>  

### Run to update the website (./docs/ folder)

``` r
pkgdown::build_site()
```

### Running roxygen

```r
roxygen2::roxygenise()
```

### TODO

See [github projects](https://github.com/systemPipeR/systemPipeShiny/projects)

</details>

