#' Overwrite a default SPS tab
#' @description If you want to load your custom content on any of the default
#' tabs in a SPS project, you can overwrite the tab with your own UI and server
#' function. First, use this function to create a template for the tab you want
#' to replace and then fill your own content.
#' @param replace_tab one of "welcome", "module_main", "vs_main", "canvas", "about",
#' for the welcome tab, module home tab, custom tab home tab, Canvas tab, about tab
#' respectively.
#' @param app_path string, where is SPS project root path
#' @param open_file bool, open the newly created template if you are in Rstudio?
#' @param overwrite bool, if the template exists, overwrite it with a new, empty one?
#'
#' @return a template file
#' @export
#'
#' @examples
#' if(interactive()){
#'     spsInit(project_name = "default_overwrite_demo",
#'             change_wd = FALSE, open_files = FALSE)
#'     ## try to run it for the first look
#'     # shiny::runApp("default_overwrite_demo")
#'     spsCoreTabReplace("welcome", app_path = "default_overwrite_demo")
#'     ## edit the file and save it.
#'     ## run again and watch the difference on the welcome tab.
#'     shiny::runApp("default_overwrite_demo")
#' }
spsCoreTabReplace <- function(
    replace_tab,
    app_path = getwd(),
    open_file = TRUE,
    overwrite = FALSE
){
    replace_tab <- match.arg(replace_tab, c("welcome", "module_main", "vs_main", "canvas", "about"))
    stopifnot(is.logical(open_file) && length(open_file) == 1)
    stopifnot(is.logical(overwrite) && length(overwrite) == 1)
    stopifnot(is.character(app_path) && length(app_path) == 1)
    if(!dir.exists(file.path(app_path, "R"))) spserror("There is no 'R' folder under your app root path")
    kword <- switch(replace_tab,
            "welcome" = "core_welcome",
            "module_main" = "module_main",
            "vs_main" = "vs_main",
            "canvas" = "core_canvas",
            "about" = "core_about"
    )

    file_out <- file.path("R", glue('tab_{kword}.R'))
    if(file.exists(file_out) && !overwrite) spserror("File exists, abort.")
    if(file.exists(file_out) && overwrite) spswarn("Over write template.")

    file_content <- glue::glue(.open = '@{', .close = '}@',
        '
        ########################## Overwrite the @{replace_tab}@ tab ###########################
        ## UI
        @{kword}@UI <- function(id){
            ns <- NS(id)
            tagList(
                # add your UI code below
            )
        }

        ## server
        @{kword}@Server <- function(id, shared){
            module <- function(input, output, session, shared){
                ns <- session$ns
                # add your server code below
            }
            moduleServer(id, module)
        }

        '
    )
    writeLines(file_content, file_out)
    msg(glue('File {file_out} created'), level = "SUCCESS", .other_color = "green")
    if(rstudioapi::isAvailable() && open_file){
        spsinfo("Now opens the file for you")
        rstudioapi::navigateToFile(file_out)
    }
}
