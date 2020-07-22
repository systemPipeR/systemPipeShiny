
# systemPipeShiny

<!-- badges: start -->
![R-CMD-check](https://github.com/systemPipeR/systemPipeShiny/workflows/R-CMD-check/badge.svg)
<!-- badges: end -->

systemPipeShiny is a framework for workflow management and data visualization. 
This tool is under devleopment, you can install it from Github.

An online demo of [SystempipeShiny](https://tgirke.shinyapps.io/systemPipeShiny/). 
This application is hosted by a small server. Please do not use it for production activities. 
Heavy tasks will crash it and disconnect you from it. 

## Installation

You can install the released version of systemPipeShiny from Github with:

``` r
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
BiocManager::install("systemPipeR/systemPipeShiny", build_vignettes=TRUE, dependencies=TRUE)
```

If you are on Linux, you also need 

```
sudo apt-get install libcurl4-openssl-dev
sudo apt-get install libv8-dev
sudo apt-get install libssl-dev
```

## Setup

To start to use SPS

``` r
library(systemPipeShiny)
spsInit()
```

Then there should be a project folder created for you. By default, it is named `SPS_`+`DATE`. 
Your working directory should be set inside that project folder automatically. 
If you are using Rstudio, three main files will be opened for you: `global.R`, `ui.R` and `server.R`. 
Now you can just run the app by type `shiny::runApp()` in console or click on the green `> Run App` 
button on top right concern of the any these 3 files in Rstudio. 
In your global.R, scroll down to the bottom, you should see:


``` r
sps_app <- sps(
    vstabs = "",
    server_expr = {
        msg("Custom expression runs -- Hello World", "GREETING", "green")
    }
)
```

This is the SPS main function. You can load/unload tabs by providing tab IDs in `vstabs` argument, like 
`c("tab1", "tab2)`. See `config/tabs.csv` in your project folder for what tabs IDs can be load and other 
tab information. 

### Load custom new tabs

We will update the instructions to create new tabs and how to load them into the framework soon ...

## Internal 

Run to update the website (./docs/ folder)

``` r
pkgdown::build_site()
```

## ToDo List
 - Documentation showing how to deploy app on user shiny.io account
 - Documentation describing all the features
  - Setting up the data
  - Launching the interface



