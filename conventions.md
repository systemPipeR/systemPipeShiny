# Coventions for this app

## 1. App structure
1. Directories:
    1. docs: `pagedown` docs, used to generate project website, can be ignored for app
    2. inst/extdata: example datasets 
    3. R: all functions
    4. vignettes: manuals for the app
    5. www: html web resources
    
## 2. Important files    
1. Three big files in the main directory:
    - **global**: all sourcing, load library, global variables.
    - **server**: top level server function, logic of global and the dash; load all submodule server functions.
    - **ui**: top level UI, load all submodule UIs.

## 3. Naming
1. submodules:
    - all store in `R` folder;
    - all should be named as `tab_xx.R`; if a submodule contains submodule, it the submodule that calls other submodules should
    be named as `tab_xx_main.R` and sub-submodules been called should be `tab_xx_xxx.R`.
    
2. functions:
    - in each *tab* file, there should be one `UI` function and one `server` function and give both functions and name space the same ID as the file name: 
    e.g. a file named `tab_sub1.R`, UI function will be `sub1UI`, server will be `sub1Server` and in top level UI and server call them `sub1UI("sub1", ...)`, `callModule(tab1Server, "tab1", ...)`.

## 4. Standard for visualization
1. file uploading:
    - Default uploading size is 5MB. Meanwhile, some files, like count table are larger than 5, in this case, need to use `Textinput` bar to specify a path or customize a function using `file.choose()`. (We can only choose one, need to be discussed)
    
2. Workflows
    - Workflows only work like dictionaries to direct users to different. 
    
3. Plots
    - In princple, datasets should be plotting ready (no need to preprocess data). 
    - For some plots that are very specific to some workflows, simple preprocess is okay.
    - Always use a button to update (re-plot) the graph, realtime rendering can be expensive. 
