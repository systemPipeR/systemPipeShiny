################## A Collections of HTML components#############################

## use on top of shiny

#' @import shiny  stringr magrittr glue htmltools
NULL


#' Use SystemPipeShiny javascripts and css style
#' call it in your header section
#' @return
#' @export
#'
#' @examples
useSps <- function(){
    # addResourcePath("sps", system.file("www", package = "systemPipeShiny"))
    addResourcePath("sps", "www")
    tags$head(
        HTML('
        <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/uikit@3.4.2/dist/css/uikit.min.css" />
        <script src="https://cdn.jsdelivr.net/npm/uikit@3.4.2/dist/js/uikit.min.js"></script>
            <script src="https://cdn.jsdelivr.net/npm/uikit@3.4.2/dist/js/uikit-icons.min.js"></script>
             '),
        tags$link(rel = "stylesheet", type = "text/css",
                            href = "sps/sps.css"),
                  tags$script(src = "sps/sps.js")

    )
}

#' Input component with a "X" button in the end to clear the entire typed text
#' work the same as `Textinput`
#' must be call `useSps` at header
#'
#' @param inputId ID
#' @param label text label above
#' @param value default value
#' @param placeholder place holder text
#'
#' @return a div
#' @export
#'
#' @examples
#' library(shiny)
#'
#' ui <- fluidPage(
#'     useSps(),
#'     clearableTextInput("input1", "This is a input box")
#' )
#'
#' server <- function(input, output, session) {
#'
#' }
#'
#' shinyApp(ui, server)
clearableTextInput <- function(inputId, label, value = "", placeholder = ""){
    tagList(tags$div(
        tags$label(label, `for` = inputId),
        tags$span(
            class = "text-input-clearable",
            style = "background-color: #f5f5f5;",
            tags$input(id = inputId, type = "text", value = value, placeholder = placeholder),
            HTML('<span id="clear_input" class="glyphicon glyphicon-remove"></span>')
            )
        ),
        tags$script(glue("clearText('{inputId}')"))
    )
}

#' Text input with a button right next to
#'
#' @param textId text box id
#' @param btnId action button id
#' @param title title of this group
#' @param label button text
#' @param icon button icon
#'
#' @return a row element
#' @export
#'
#' @examples
textInputGroup <- function(textId, btnId, title="", label="", icon = "paper-plane"){
    fluidRow(
        column(
            9, style = "padding-right: 0; bottom: 10px",
            clearableTextInput(inputId = textId, label = title)
               ),
            column(3, style = "padding-left: 10px; bottom: 10px",
                   br(),
                   actionButton(btnId, label = label, icon(icon)))
    )
}



#' A shiny gallery component
#' `texts`, `hrefs`, `images` Must have the same length
#' Must be used with Bootstrap3 and sps.css file
#' @importFrom assertthat assert_that
#' @param Id ID of this gallery
#' @param title Title of gallery
#' @param texts label under each image
#' @param hrefs link when clicking each
#' @param image_frame_size integer, 1-12
#' @param img_height css style like '300px'
#' @param img_width css style like '480px'
#' @param images image source,
#'
#' @return a div element
#'
#' @examples
#' texts <- c("p1", "p2", "p3", "p4", "p5")
#' hrefs <- c("https://unsplash.it/1200/768.jpg?image=251",
#'            "https://unsplash.it/1200/768.jpg?image=252",
#'            "https://unsplash.it/1200/768.jpg?image=253",
#'            "https://unsplash.it/1200/768.jpg?image=254",
#'            "https://unsplash.it/1200/768.jpg?image=255")
#' images <- c("https://unsplash.it/600.jpg?image=251",
#'             "https://unsplash.it/600.jpg?image=252",
#'             "https://unsplash.it/600.jpg?image=253",
#'             "https://unsplash.it/600.jpg?image=254",
#'             "https://unsplash.it/600.jpg?image=255")
#' library(shiny)
#'
#' ui <- fluidPage(
#'     includeCSS("www/sps.css"),
#'     gallery(texts = texts, hrefs = hrefs, images = images, image_size = 2)
#' )
#'
#' server <- function(input, output, session) {
#'
#' }
#'
#' shinyApp(ui, server)
gallery <- function(Id = NULL, title = "Gallery", title_color = "#0275d8", texts,
                    hrefs, images, image_frame_size = 4, img_height = "300px",
                    img_width = "480px"
                    ){
    if (is.null(Id)) Id <- glue("gallery{sample(1000:100000, 1)}")
    assert_that(length(texts) == length(hrefs) & length(hrefs) == length(images),
                msg = "texts, hrefs and images must have the same length")
    tags$div(
        id = Id, class = "col uk-overflow-hidden uk-overlay", `uk-scrollspy`="cls: uk-animation-slide-bottom; target: img; delay: 300; repeat: true",
        p(class = "text-center h2", style = glue("color: {title_color};"), title),
        tags$div(
            class = "row",
            HTML(glue('
                <a href="{hrefs}"  class="col-sm-{image_frame_size} style="right: 5px;">
                  <img src="{images}" class="img-gallery" style="height: {img_height}; width: {img_width};">
                  <p class="text-center h4">{texts}</p>
                </a>
             '))
                 )
    )
}


#' generate gallery by only providing tab names
#'
#' @param tabnames tab names
#' @param Id div ID
#' @param title gallery title
#' @param title_color title color
#' @param image_frame_size integer, 1-12
#' @param img_height css style like '300px'
#' @param img_width css style like '480px'
#'
#' @return gallery div
#'
#' @example
#' library(shiny)
#' ui <- fluidPage(
#'     genGallery(c("tab_a", "tab_b"))
#' )
#' server <- function(input, output, session) {
#'
#' }
#' shinyApp(ui, server)
genGallery <- function(tabnames, Id = NULL, title = "Gallery",
                       title_color = "#0275d8", image_frame_size = 4, img_height = "300px",
                       img_width = "480px") {
    tabs <- findTabInfo(tabnames)
    gallery(Id = Id, title = title, title_color = title_color,
            image_frame_size = image_frame_size, img_height = img_height,
            texts = tabs[['tab_labels']], hrefs = tabs[['hrefs']],
            images = tabs[['images']])
}

#' Show a list of tabs in buttons
#' `label_text`, `hrefs` must be the same length
#' @importFrom assertthat assert_that
#' @param Id optional
#' @param title Item title
#' @param title_color title color
#' @param label_text individual tab labels
#' @param hrefs individual tab links
#'
#' @return a div element
#' @export
#'
#' @examples
#' ui <- fluidPage(
#' includeCSS("www/sps.css"),
#' hrefTab(label_text = c("Bar Plot", "PCA Plot", "Scatter Plot"), hrefs = c("https://google.com/", "", ""))
#' )
#'
#' server <- function(input, output, session) {
#'
#' }
#'
#' shinyApp(ui, server)
hrefTab <- function(Id = NULL, title = "A list of tabs", title_color = "#0275d8",
                    label_text, hrefs, ...
                    ){
    if (is.null(Id)) Id <- glue("list-tab{sample(1000:100000, 1)}")
    assert_that(length(label_text) == length(hrefs),
                msg = "texts and hrefs must have the same length")
    tags$div(
        id = Id, class = "col", ... ,
        p(class = "h4",  style = glue("color: {title_color}; text-align: left;"), title),
        tags$div(
            HTML(glue('
                <a href="{hrefs}" class="href-button">{label_text}</a>\n
                '))
        )
    )
}


#' A table of lists of hyper reference buttons
#' `item_titles`, `item_labels`, `item_hrefs` must have the same length
#' nth item in `item_labels`, `item_hrefs` must have the same length
#' @importFrom assertthat assert_that
#' @param Id optional
#' @param title title of this table
#' @param text_color text color
#' @param item_titles vector string, a vector of titles for table items
#' @param item_labels list, a list of button lables in each table item
#' @param item_hrefs list, a list of hrefs for each button
#' @param ... other HTML args
#'
#' @return
#' @export
#'
#' @examples
#' library(shiny)
#'
#' ui <- fluidPage(
#'     includeCSS("www/sps.css"),
#'     hrefTable(item_titles = c("workflow 1", "workflow 2"),
#'               item_labels = list(c("tab 1"), c("tab 3", "tab 4")),
#'               item_hrefs = list(c("https://www.google.com/"), c("", "")),
#'     )
#'
#' )
#'
#' server <- function(input, output, session) {
#'
#' }
#'
#' shinyApp(ui, server)
hrefTable <- function(Id = NULL, title = "A Table of list of tabs",
                      text_color = "#0275d8", item_titles, item_labels,
                      item_hrefs, ...
                      ){
    if (is.null(Id)) Id <- glue("list-table{sample(1000:100000, 1)}")
    assert_that(is.list(item_labels)); assert_that(is.list(item_hrefs))
    assert_that(length(item_titles) == length(item_labels) & length(item_labels) == length(item_hrefs),
                msg = "item_titles, item_labels and item_hrefs must have the same length")
    mapply(
        function(label, href) {
            assert_that(length(href) == length(label),
                        msg = paste0("'", paste0(label, collapse = ", "),
                                     "' must have the same length as '",
                                     paste0(href, collapse = ", "), "'")
                        )
            },
        item_labels, item_hrefs
        )
    btns <- mapply(
        function(label, href) {
            glue('<a href="{href}" class="href-button">{label}</a>') %>% glue_collapse()
            },
        item_labels, item_hrefs
        )
    tags$table(
        id = Id, class = "table table-hover table-href table-striped",
        tags$caption(class = "text-center h2",  style = glue("color: {text_color};"), title),
        HTML('<thead>
                <tr class="info">
                  <th>Category</th>
                  <th>Options</th>
                </tr>
              </thead>'),
       tags$tbody(HTML(glue(
       '
          <tr>
            <td class="h4" style="color: {text_color};">{item_titles}</td>
            <td>{btns}</td>
          </tr>\n
        '
        ))),
    )
}

#' generate a table of lists of hyper reference buttons by using tab config file
#' @param rows a list of rows, each item name in the list will be the row name,
#' each item is a vector of tab names
#'
#' @param Id element ID
#' @param title table title
#' @param text_color text color for table
#' @param ... any additional args to the html element, like class, style ...
#'
#' @example
#' library(shiny)
#' rows <- list(wf1 = c("df_raw", "df_count"), wf2 =  c("df_raw"))
#' ui <- fluidPage(
#'     genHrefTable(rows)
#' )
#' server <- function(input, output, session) {
#'
#' }
#' shinyApp(ui, server)
genHrefTable <- function(rows, Id = NULL, title = "A Table of list of tabs",
                         text_color = "#0275d8", ...) {
    tab_list <- sapply(rows, function(x) findTabInfo(x))
    hrefTable(Id = Id, title = title,
              text_color = text_color, item_titles = names(rows),
              item_labels = tab_list[1,], item_hrefs = tab_list[2,], ...)
}



#' Render tab description
#'
#' @param desc one string in markdown format
#'
#' @examples
#' desc <-
#' "
#' # Some desc
#' - xxxx
#' "
#' renderDesc(desc)
renderDesc <- function(desc) {
    HTML(markdown::renderMarkdown(text = glue(desc)))
}


#' render the app UI
#' Combine mainUI defined in ui.R and add the loading screen and more
#' @param mainUI a normal shiny page ui
#'
#' @return a `fluidPage`
#'
#' @examples
#' ui <- fluidPage()
#' server <- function(input, output, session) {}
#' mainUI <- shinyApp(ui, server)
#' spsUI(mainUI)
spsUI <- function(mainUI){
    fluidPage(
        if (getOption("sps")$loading_screen)
        {tagList(
            div(id = "app-main", style = "margin-left: -2em; margin-right: -2em; height:auto;", class = "shinyjs-hide",
                mainUI
            ),
            div(id = "loading-screen", style="height: 100vh; width: 100vw; overflow: hidden;",
                    sytle="z-index:100; position:absolute;",
                    tags$style('
                    #toapp{
                      border: none;
                      background: none;
                      z-index:100;
                        top: 85%;
                        left: 50%;
                      position:absolute;
                      display:block;
                      transform: translate(-50%, -50%);
                    }
                    #toapp:active {
                      transform: translate(-50%, -10%);
                    }
                               '),
                    actionBttn(inputId = "toapp", "Continue to app", icon = icon("angle-double-right"), size = "lg"),
                    renderLoading()
            )
        )}
        else
        {
            div(id = "app-main", style='position: absolute; left: 0px; top: 0px; right: 0px; bottom: 0px;', mainUI)
        }
    )
}


#' dynamically generate select file input
#' depending on the mode in options, render similar UI but server side works
#' differently. `local` mode will not copy file, directly use a path pointer,
#' `server` mode upload file and store in temp. Expect similar behavior as
#' `fileInput`.
#' @param id element id
#' @param title element title
#' @param label upload button label
#' @param icon button icon, only works for `local` mode
#' @param style additional button style, only works for `local` mode
#' @param multiple multiple files allowed
#' `shinyFilesButton`
#'
#' @return div
#' @export
#'
#' @examples
#' library(shiny)
#' library(shinyFiles)
#' library(shinyjs)
#' options(sps = list(mode='server'))
#' ui <- fluidPage(
#' useShinyjs(),
#' dynamicFile("getFile"),
#' textOutput("txt_file")
#' )
#'
#' server <- function(input,output,session){
#'     runjs('$(".sps-file input").attr("readonly", true)')
#'     myfile <- dynamicFileServer(input,output,session, id = "getFile")
#'     observe({
#'         print(myfile())
#'     })
#' }
#' shinyApp(ui = ui, server = server)
dynamicFile <- function(id, title = "Select your file:",
                        label = "Browse", icon = NULL, style = "",
                        multiple = FALSE){
    icon <- if(is.empty(icon)) icon("upload")
    if (getOption("sps")$mode == "local") {
        div(class = "form-group shiny-input-container sps-file",
            tags$label(class="control-label", `for`=id, title),
            div(class="input-group",
                tags$label(class="input-group-btn input-group-prepend",
                           shinyFilesButton(id, label,
                                            title = title, multiple = multiple,
                                            buttonType = "btn btn-primary", icon = icon,
                                            style = style)
                ),
                textInput(inputId = glue("{id}-text"), label = NULL, placeholder="No file selected")
            )

        )

    } else {
        fileInput(inputId = id, label = title,
                  multiple = multiple, buttonLabel = label)
    }
}
