
# systemPipeShiny

<!-- badges: start -->
![R-CMD-check](https://github.com/systemPipeR/systemPipeShiny/workflows/R-CMD-check/badge.svg)
<!-- badges: end -->

This tool is under devleopment, you can install it from Github

An online demo of [SystempipeShiny](https://tgirke.shinyapps.io/systemPipeShiny/). This application is hosted by a small server. Please do not use it for production activities. Heavy tasks will crash it and disconnect you from it. 

## Installation

You can install the released version of systemPipeShiny from Github with:

``` r
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
BiocManager::install("systemPipeR/systemPipeShiny", build_vignettes=TRUE, dependencies=TRUE)
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(systemPipeShiny)
## basic example code
## To be added...
```

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

# For developers
  
## Coventions for this app

### 1. App structure
1. Directories:
    1. docs: `pagedown` docs, used to generate project website, can be ignored for app
    2. data: example datasets 
    3. R: all functions, sub-tabs. This folder will be automatically sourced
    4. vignettes: manuals for the app, can be ignored for app
    5. www: html web resources, will be treated as root of front-end resources
    
### 2. Important files    
1. Three big files in the main directory:
    - **global**: all sourcing, load library, global variables.
    - **server**: top level server function, logic of global and the dash; load all submodule server functions.
    - **ui**: top level UI, load all submodule UIs.

### 3. Naming
1. tabs:
    - All store in `R` folder;
    - All should be named as `tab_xx.R`; if a submodule contains submodule. If 
    this tab is a sub tab, name it `tab_UPPER_LOWER.R`, e.g. a tab for 
    visualization new data type will be `tab_vs_df_xxx.R`, a new plot tab will be 
    `tab_vs_plot_xxx.R`.
    - All tab info should also be updated in `tabs.csv` as the tab metadata.
        - visualization data tabs should hava tab name as `df_xx`, plot tab 
        should be `plot_xx`
    
2. functions:
    - in each *tab* file, there should be one `UI` function and one `server` 
    function and give both functions and name space the same ID as the file name: 
    e.g. a file named `tab_sub1.R`, UI function will be `sub1UI`, server will be 
    `sub1Server` and in top level UI and server call them `sub1UI("sub1", ...)`, 
    `callModule(tab1Server, "tab1", ...)`.

## 4. Standard for visualization
1. Plots
    - In princple, datasets should be plotting ready (no need to preprocess data). 
    - For some plots that are very specific to some workflows, simple preprocess is okay.
    - Always use a button to update (re-plot) the graph, realtime rendering can be expensive. 

## 5. Objects saved in `shared`

"Shared" is first defined in the `server.R` as an object to hold data that can
be passed around modules. This is very important if you want to transfer like a 
dataframe from df tabs to plotting tabs. 

To access values in `shared`, use `$`, e.g. `shared$xxx$subxxx`.

### default stored objects

- wf_flags: bool values to indicate required files status for the workflow
    - targets_ready, wf_ready, wf_conf_ready: target file, workflow Rmd file, config yaml file
    
- targets: targets file
    - df: dataframe, which will be used to display from top push bar
    - file: string, the temp path of edited targets. When `add to task` is clicked and 
    check passed, this file will be write to temp with **target header**
    
- count: count table
    - df, file: same as targets
    
- config: configuration yaml file
    - file: same as targets

