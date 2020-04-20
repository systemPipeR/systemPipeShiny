################## A Collections of HTML components#############################

## use on top of shiny



# input with a "X" button in the end to clear the entire typed text 
# work the same as Textinput
# must be used together with the js and css file
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
#' @param Id ID of this gallery
#' @param title Title of gallery
#' @param texts label under each image
#' @param hrefs link when clicking each
#' @param images image source, 
#' @param image_size size of each image 1 to 12
#'
#' @return a div element
#' @export
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
gallery <- function(Id = NULL, title = "Gallery", texts, hrefs, images, image_size = 4){
    if (is.null(Id)) Id <- glue("gallery{sample(1000:10000, 1)}")
    assert_that(length(texts) == length(hrefs) & length(hrefs) == length(images), 
                msg = "texts hrefs and images must have the same length")
    tags$div(
        id = Id, class = "col",
        p(class = "text-center h2", title),
        tags$div(
            class = "row",
            HTML(glue('
                <a href="{hrefs}"  class="col-sm-{image_size} style="right: 5px;" >
                  <img src="{images}" class="img-gallery">
                  <p class="text-center h4">{texts}</p>
                </a>
             '))
                 )
    ) %>% return()
}

