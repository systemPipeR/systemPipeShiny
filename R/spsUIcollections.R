################## A Collections of HTML components#############################

## use on top of shiny



#' Input component with a "X" button in the end to clear the entire typed text
#' work the same as `Textinput`
#' must be used together with the js and css file
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

# Text input with a button next to
# textId: text box id
# btnId: action button id
# title: title of this group
# label: button text 
# icon: btn icon
textInputGroup <- function(textId, btnId, title="", label="", icon = "paper-plane"){
    fluidRow( 
        column(9, style = "padding-right: 0; bottom: 10px", clearableTextInput(inputId = textId, label = title)),
            column(3, style = "padding-left: 10px; bottom: 10px",
                   br(),
                   actionButton(btnId, label = label, icon(icon)))
    )
}
        


#' A shiny gallery component
#' `texts`, `hrefs`, `images` Must have the same length
#' Must be used with Bootstrap3 and sps.css file
#'
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
gallery <- function(Id = NULL, title = "Gallery", title_color = "#0275d8", texts, hrefs, images, image_frame_size = 4, img_height = "300px", img_width = "480px"){
    if (is.null(Id)) Id <- glue("gallery{sample(1000:100000, 1)}")
    assert_that(length(texts) == length(hrefs) & length(hrefs) == length(images), 
                msg = "texts, hrefs and images must have the same length")
    tags$div(
        id = Id, class = "col",
        p(class = "text-center h2", style = glue("color: {title_color};"), title),
        tags$div(
            class = "row",
            HTML(glue('
                <a href="{hrefs}"  class="col-sm-{image_frame_size} style="right: 5px;" >
                  <img src="{images}" class="img-gallery" style="height: {img_height}; width: {img_width};">
                  <p class="text-center h4">{texts}</p>
                </a>
             '))
                 )
    )
}


#' Show a list of tabs in buttons
#' `label_text`, `hrefs` must be the same length
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
#' TabHref(label_text = c("Bar Plot", "PCA Plot", "Scatter Plot"), hrefs = c("https://google.com/", "b", "c"))
#' )
#' 
#' server <- function(input, output, session) {
#' 
#' }
#' 
#' shinyApp(ui, server)
TabHref <- function(Id = NULL, title = "A list of tabs", title_color = "#0275d8", label_text, hrefs, ...) {
    if (is.null(Id)) Id <- glue("list-tab{sample(1000:100000, 1)}")
    assert_that(length(label_text) == length(hrefs), 
                msg = "texts and hrefs must have the same length")
    tags$div(
        id = Id, class = "col tab-group", ... ,
        p(class = "text-center h4",  style = glue("color: {title_color}; text-align: left;"), title),
        tags$div(
            HTML(glue('
                <a href="{hrefs}" class="tab-href">{label_text}</a>\n
                '))
        )
    )
}


#' A table of lists of hyper reference buttons 
#' `item_titles`, `item_labels`, `item_hrefs` must have the same length
#' nth item in `item_labels`, `item_hrefs` must have the same length
#' @param Id optional
#' @param title title of this table
#' @param text_color text color
#' @param item_titles vector string, a vector of titles for table items
#' @param item_labels list, a list of button lables in each table item
#' @param item_hrefs list, a llist of hrefs for each button
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
#'     TableHref(item_titles = c("worflow 1", "workflow 2"),
#'               item_labels = list(c("tab 1", "tab 2"), c("tab 3", "tab 4")),
#'               item_hrefs = list(c("https://www.google.com/", ""), c("", "")),
#'     )
#'     
#' )
#' 
#' server <- function(input, output, session) {
#'     
#' }
#' 
#' shinyApp(ui, server)
TableHref <- function(Id = NULL, title = "A Table of list of tabs", text_color = "#0275d8", item_titles, item_labels, item_hrefs, ...) {
    if (is.null(Id)) Id <- glue("list-table{sample(1000:100000, 1)}")
    assert_that(is.list(item_labels)); assert_that(is.list(item_hrefs))
    assert_that(length(item_titles) == length(item_labels) & length(item_labels) == length(item_hrefs), 
                msg = "item_titles, item_labels and item_hrefs must have the same length")
    tags$div(
        id = Id, class = "col",
        p(class = "text-center h2",  style = glue("color: {text_color};"), title),
        tags$hr(style="height:1px; background-color:gray;"),
        lapply(1:length(item_titles), function(n) {
            TabHref(title = item_titles[n], title_color = text_color, label_text = item_labels[[n]], hrefs = item_hrefs[[n]], class = "table-href")
        })    
    )
}    
