# To deploy: 
options(repos = BiocManager::repositories())
getOption("repos")
options(shiny.reactlog = TRUE) # for debugging
rsconnect::deployApp()