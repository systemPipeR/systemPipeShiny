
# systemPipeShiny <img src="https://github.com/systemPipeR/systemPipeShiny-book/blob/master/img/sps.png?raw=true" align="right" height="139" />

<!-- badges: start -->
![R-CMD-check](https://github.com/systemPipeR/systemPipeShiny/workflows/R-CMD-check/badge.svg)
<!-- badges: end -->

`systemPipeShiny`(SPS) a Shiny-based R/Bioconductor package that forms a framework for workflow management and data visualization. 


There is an online demo of [systempipeShiny](https://tgirke.shinyapps.io/systemPipeShiny/). 
This application is hosted by a small server. Do not use it for production activities. 
Heavy tasks will crash it and disconnect you from it. 

## Installation

You can install the released version of `systemPipeShiny`:

``` r
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
BiocManager::install("systemPipeShiny")
```
Develop version

``` r
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
BiocManager::install("systemPipeR/systemPipeShiny")
```


If you are on Linux, you also need following. Different distributions may have different 
commands, Ubuntu for example:

```
sudo apt-get install libcurl4-openssl-dev
sudo apt-get install libv8-dev
sudo apt-get install libxm12-dev
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
If you are using Rstudio, *global.R* file will be opened for you and this is the 
only file you need to make custom changes, if there is any.
Now you can just run the app by type `shiny::runApp()` in console or click on the green `> Run App` 
button on top right corner of the any these 3 files in Rstudio. 
In your *global.R*, scroll down to the bottom, you should see:


``` r
sps_app <- sps(
    vstabs = "",
    plugin = "",
    server_expr = {
        msg("Custom expression runs -- Hello World", "GREETING", "green")
    }
)
```

This is the SPS main function. You can load/unload tabs by providing tab IDs in `vstabs` argument, like 
`c("tab1", "tab2)`. See *config/tabs.csv* or use `spsTabInfo()` in your project 
folder for what tabs IDs can be load and other tab information. 

### Load a plugin 
SPS plugins usually are a collection of tabs, and they are distributed as normal 
R packages. First, you need to install the plugin using `install.packages`, `remotes` or 
`BiocManager`. Then under the app directory use `spsAddPlugin("PLUGIN_NAME")` to 
load the plugin.

The current only option is `spsBio`. To install, run `BiocManager::install("systemPipeR/spsBio")`. To load, run `spsAddPlugin("spsBio")`.
In your *global.R* add it to the `plugin` argument

``` r
sps_app <- sps(
    vstabs = "",
    plugin = "spsBio",
    server_expr = {
        msg("Custom expression runs -- Hello World", "GREETING", "green")
    }
)
```

### Load custom new tabs

After you have created your SPS project by the `spsInit` function, you can use `newTabData` to create a data tab 
and use `newTabPlot` to create a plot tab.

```r
newTabData(
    tab_id = "data_new", 
    tab_displayname = "my first data tab",
    prepro_methods = list(makePrepro(label = "do nothing",
                                     plot_options = "plot_new"))
)
newTabPlot(
    tab_id = "plot_new",
    tab_displayname = "my first plot tab",
    plot_data = list(makePlotData(dataset_label = "Data from my new tab",
                                  receive_datatab_ids = "data_new"))
           )
```
This code should generate a new data tab called *data_new* with a label *my first data tab* (what 
you see on the UI), and a new plot tab `plot_new`.

The important arg `plot_options = "plot_new"` is saying this data tab can make a plot, and the 
plot tab ID is "plot_new". On the plot tab similarly, `receive_datatab_ids = "data_new"` tells 
the framework this plot tab need to receive data from data tab `data_new`. In this way, we connect 
the data tab and the plot tab with each other. Of course, one data tab can link to multiple 
plot tabs and a plot tab can receive data from multiple data tabs too. Just specify the 
tab IDs by a vector `c(xx, xx)`.

Tabs are not loaded at this point, you need to specify you do want to load them by adding them 
to the app main function on the `global.R` file. Then launch the app as usually. New tab files 
are automatically created under your R folder, registered to your `config/tabs.csv` and 
sourced automatically.

``` r
sps_app <- sps(
    vstabs = c("data_new", "plot_new"), # add new tab IDs here
    server_expr = {
        msg("Custom expression runs -- Hello World", "GREETING", "green")
    }
)
```
If you don't want any tab file, use `removeSpsTab("TAB_ID")` to remove a tab. It will remove the R 
file and delete records in your `config/tabs.csv` file. 


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

