################## A Collections of HTML components#############################
# can be used outside SPS framework, like other shiny projects
## use on top of shiny

#' Use SystemPipeShiny javascripts and css style
#' @description call it in your head section of your shiny UI. Adding this to
#' the Shiny app UI is required for most SPS shiny widgets. This is required for
#' using SPS widgets outside of SPS framework. No need to load this function
#' again if you are working within SPS framework.
#' @importFrom shinytoastr useToastr
#' @importFrom bsplus use_bs_popover use_bs_tooltip
#' @export
#' @return HTML head
#' @examples
#' # useSps()
useSps <- function(){
    addResourcePath("sps", "www")
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css",
                            href = "sps/css/sps.css"),
        tags$script(src = "sps/js/sps.js"),
        tags$script(src="sps/js/sps_update_pg.js"),
        tags$script(src = "sps/js/sps_gotop.js"),
        shinytoastr::useToastr(),
        shinyjs::useShinyjs(),
        bsplus::use_bs_popover(),
        bsplus::use_bs_tooltip()
    )
}

#' A clearable text inputInput control
#' @description An UI component with a "X" button in the end to clear the entire
#' entered text. It works the same as `Textinput`.
#' @details must be call [useSps()] at head to load css and js
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
#' if(interactive()){
#'
#'     ui <- fluidPage(
#'         useSps(),
#'         clearableTextInput("input1", "This is a input box")
#'     )
#'
#'     server <- function(input, output, session) {
#'
#'     }
#'
#'     shinyApp(ui, server)
#' }
clearableTextInput <- function(inputId, label, value = "", placeholder = ""){
    tagList(tags$div(
        tags$label(label, `for` = inputId),
        tags$span(
            class = "text-input-clearable",
            style = "background-color: #f5f5f5;",
            tags$input(
                id = inputId,
                type = "text",
                value = value,
                placeholder = placeholder
            ),
            HTML('<span id="clear_input"
                 class="glyphicon glyphicon-remove"></span>')
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
#' if(interactive()){
#'
#'     ui <- fluidPage(
#'         useSps(),
#'         textInputGroup("id1", "id2")
#'     )
#'
#'     server <- function(input, output, session) {
#'
#'     }
#'
#'     shinyApp(ui, server)
#' }
textInputGroup <- function(textId, btnId, title="",
                           label="", icon = "paper-plane"){
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
#' @description `texts`, `hrefs`, `images` Must have the same length
#' @details [useSps()] must be called in UI before using this function.
#'
#' The version usually been used in SPS framework is [genGallery()]
#' @importFrom assertthat assert_that
#' @param Id ID of this gallery
#' @param title Title of gallery
#' @param title_color Title color
#' @param texts label under each image
#' @param hrefs link when clicking each
#' @param image_frame_size integer, 1-12, this controls width
#' @param images image source,
#' @export
#' @return a div element
#'
#' @examples
#' if(interactive()){
#'     texts <- c("p1", "p2", "p3", "p4", "p5")
#'     hrefs <- c("https://unsplash.it/1200/768.jpg?image=251",
#'                "https://unsplash.it/1200/768.jpg?image=252",
#'                "https://unsplash.it/1200/768.jpg?image=253",
#'                "https://unsplash.it/1200/768.jpg?image=254",
#'                "https://unsplash.it/1200/768.jpg?image=255")
#'     images <- c("https://unsplash.it/600.jpg?image=251",
#'                 "https://unsplash.it/600.jpg?image=252",
#'                 "https://unsplash.it/600.jpg?image=253",
#'                 "https://unsplash.it/600.jpg?image=254",
#'                 "https://unsplash.it/600.jpg?image=255")
#'     library(shiny)
#'
#'     ui <- fluidPage(
#'         useSps(),
#'         gallery(texts = texts, hrefs = hrefs, images = images)
#'     )
#'
#'     server <- function(input, output, session) {
#'
#'     }
#'
#'     shinyApp(ui, server)
#' }
gallery <- function(Id = NULL, title = "Gallery",
                    title_color = "#0275d8", texts,
                    hrefs, images, image_frame_size = 4){
    if (is.null(Id)) Id <- glue("gallery{sample(1000000:9999999, 1)}")
    assert_that(
        length(texts) == length(hrefs) & length(hrefs) == length(images),
        msg = "texts, hrefs and images must have the same length")
    tags$div(
        id = Id, class = "col sps-gallery",
        p(class = "text-center h2",
          style = glue("color: {title_color};"),
          title),
        tags$div(
            class = "row", style = "  margin: 10px;",
            HTML(glue('
                <a href="{hrefs}"  class="col-sm-{image_frame_size} sps-tab-link" style="right: 1px;">
                  <img src="{images}" class="img-gallery" height=300 width=400 style="width: 100%;">
                  <p class="text-center h4">{texts}</p>
                </a>
             '))
                 )
    )
}


#' Display a list of links in a row of buttons
#' @description `hrefTab` can be use for any purpose of shiny.
#' `genHrefTab` is upper level wrapper of `hrefTab` and should
#' only be used under SPS framework for fast retrieving tab info and
#' generate the `hrefTab`. To use `genHrefTab`, the `tab_info.csv` config file
#' must be there in `config` directory.
#' `label_text`, `hrefs` must be the same length
#' @importFrom assertthat assert_that
#' @param Id optional element ID
#' @param title Item title
#' @param title_color title color
#' @param label_text individual tab labels
#' @param hrefs individual tab links
#' @param ... other arguments to be passed to the html element
#' @return a HTML element
#' @details *genHrefTab* require a SPS project and the config/tabs.csv file.
#' If you want to use hrefTab outside a SPS project just use *hrefTab*
#' @export
#' @examples
#' if(interactive()){
#'     ui <- fluidPage(
#'         useSps(),
#'         hrefTab(label_text = c("Bar Plot", "PCA Plot", "Scatter Plot"),
#'                 hrefs = c("https://google.com/", "", ""))
#'     )
#'
#'     server <- function(input, output, session) {
#'
#'     }
#'     shinyApp(ui, server)
#' }
hrefTab <- function(Id = NULL, title = "A list of tabs",
                    title_color = "#0275d8",
                    label_text, hrefs, ...
                    ){
    if (is.null(Id)) Id <- glue("list-tab{sample(1000000:9999999, 1)}")
    assert_that(length(label_text) == length(hrefs),
                msg = "texts and hrefs must have the same length")
    tags$div(
        id = Id, class = "col", ... ,
        p(class = "h4",
          style = glue("color: {title_color}; text-align: left;"),
          title),
        tags$div(
            HTML(glue('
            <a href="{hrefs}" class="href-button
            sps-tab-link">{label_text}</a>\n
                '))
        )
    )
}

#' A table of hyper reference buttons
#' @description creates a table in Shiny which the cells are hyper reference
#' buttons
#' @details `item_titles`, `item_labels`, `item_hrefs` must have the same
#' length. Each item in `item_labels`, `item_hrefs` must also have the same
#' length. For example, if we want to make a table of two rows, the first row
#' has 1 cell and the second row has 2 cells:
#'
#' ```
#'  hrefTable(
#'      item_titles = c("row 1", "row 2"),
#'      item_labels = list(c("cell 1"), c("cell 1", "cell 2")),
#'      item_hrefs = list(c("link1"), c("link1", "link2")
#'  )
#' ```
#'
#' Must call [useSps()] in UI.
#'
#' The more often used versuin in SPS framework is [genHrefTable()]
#' @importFrom assertthat assert_that
#' @param Id optional
#' @param title title of this table
#' @param text_color text color
#' @param item_titles vector of strings, a vector of titles for table row names
#' @param item_labels list, a list of character vectors to specify button
#' labels in each table item
#' @param item_hrefs list, a list  of character vectors to specify button hrefs
#' links
#' @param ... other HTML args
#' @export
#' @return HTML elements
#' @examples
#' if(interactive()){
#'
#'     ui <- fluidPage(
#'         useSps(),
#'         hrefTable(item_titles = c("workflow 1", "workflow 2"),
#'                   item_labels = list(c("tab 1"), c("tab 3", "tab 4")),
#'                   item_hrefs = list(c("https://www.google.com/"), c("", "")),
#'         )
#'
#'     )
#'
#'     server <- function(input, output, session) {
#'
#'     }
#'
#'     shinyApp(ui, server)
#' }
hrefTable <- function(Id = NULL, title = "A Table of list of tabs",
                      text_color = "#0275d8", item_titles, item_labels,
                      item_hrefs, ...
                      ){
    if (is.null(Id)) Id <- glue("list-table{sample(1000000:9999999, 1)}")
    assert_that(is.list(item_labels)); assert_that(is.list(item_hrefs))
    assert_that(length(item_titles) == length(item_labels) &
                    length(item_labels) == length(item_hrefs),
                msg = glue("item_titles, item_labels and ",
                            "item_hrefs must have the same length"))
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
            glue('<a href="{href}" class="href-button ',
                 'sps-tab-link">{label}</a>') %>%
                glue_collapse()
            },
        item_labels, item_hrefs
        )
    tags$table(
        id = Id, class = "table table-hover table-href table-striped",
        tags$caption(class = "text-center h2",
                     style = glue("color: {text_color};"),
                     title),
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


#' Render some collapsible markdown text
#' @description write some text in markdown format and it will help you
#' render to a collapsible markdown section on Shiny UI
#' @param desc one string in markdown format
#' @param id HTML ID
#' @importFrom markdown renderMarkdown
#' @export
#' @return HTML elements
#' @examples
#' if(interactive()){
#'     library(shiny)
#'     desc <-
#'         "
#'     # Some desc
#'     - xxxx
#'     - bbbb
#'
#'     `Some other things`
#'     > other markdown things
#'     1. aaa
#'     2. bbb
#'     3. ccc
#'     "
#'     ui <- fluidPage(
#'         useSps(),
#'         renderDesc(id = "desc", desc)
#'     )
#'
#'     server <- function(input, output, session) {
#'
#'     }
#'
#'     shinyApp(ui, server)
#' }
renderDesc <- function(id, desc) {
    HTML(glue('
      <div class="desc">
        <div class="collapse desc-body" id="{id}" aria-expanded="false">
         {HTML(markdown::renderMarkdown(text = glue(desc, .open = "@{", .close = "}@")))}
        </div>

        <a role="button" class="collapsed" data-toggle="collapse"
           href="#{id}" aria-expanded="false" aria-controls="{id}">
        </a>
      </div>
    '))
}

#' Dynamically generate Shiny file selection component
#' @description  Depending on the "mode" in SPS options, this function renders
#' a similar UI components but behaves differently on server.
#' `local` mode will not copy file, directly
#' use a path pointer, `server` mode upload file and store in temp. Expect
#' similar behavior as [shiny::fileInput].
#' @param id element ID
#' @param title element title
#' @param label upload button label
#' @param icon button icon, only works for `local` mode
#' @param style additional button style, only works for `local` mode
#' @param multiple multiple files allowed
#' @importFrom shinyAce is.empty
#' @importFrom shinyFiles shinyFilesButton
#' @return a Shiny upload component
#' @export
#'
#' @examples
#' if(interactive()){
#'     library(shinyjs)
#'     # change to 'local' to see the difference
#'     options(sps = list(mode='server'))
#'     ui <- fluidPage(
#'         useShinyjs(),
#'         dynamicFile("getFile"),
#'         textOutput("txt_file")
#'     )
#'
#'     server <- function(input,output,session){
#'         runjs('$(".sps-file input").attr("readonly", true)')
#'         myfile <- dynamicFileServer(input,session, id = "getFile")
#'         observe({
#'             print(myfile())
#'         })
#'     }
#'     shinyApp(ui = ui, server = server)
#' }
dynamicFile <- function(id, title = "Select your file:",
                        label = "Browse", icon = NULL, style = "",
                        multiple = FALSE){
    icon <- if(shinyAce::is.empty(icon)) icon("upload")
    if (spsOption('mode') == "local") {
        div(class = "form-group shiny-input-container sps-file",
            tags$label(class="control-label", `for`=id, title),
            div(class="input-group",
                tags$label(class="input-group-btn input-group-prepend",
                           shinyFiles::shinyFilesButton(id, label,
                                            title = title, multiple = multiple,
                                            buttonType = "btn btn-primary",
                                            icon = icon,
                                            style = style)
                ),
                textInput(inputId = glue("{id}-text"), label = NULL,
                          placeholder="No file selected", width = "100%")
            )

        )

    } else {
        fileInput(inputId = id, label = title,
                  multiple = multiple, buttonLabel = label)
    }
}

#' Example UI elements for plotting
#' @description return some example UI elements can be toggled on plotting.
#'  This functions is only used as a temp solution for the example tab to
#'  demonstrate what UI components
#'  you can use. Will be removed in later releases as we have a better tab
#'  organizations.
#' @param ns namespace function
#'
#' @return some UI
#' @importFrom shinyAce aceEditor
#' @importFrom shinydashboardPlus boxPlus
#' @importFrom shinyWidgets dropdownButton tooltipOptions
#' @importFrom shinyWidgets  radioGroupButtons sliderTextInput
#' @export
#' @examples
#' if(interactive()){
#'     ui <- fluidPage(useSps(), uiExamples(NS("example")))
#'     server <- function(input, output, session) {}
#'     shinyApp(ui, server)
#' }
uiExamples <- function(ns){
    tagList(
        h4("Some examples for plotting controls"),
        shinydashboardPlus::boxPlus(
            width = 12, closable = FALSE, collapsible = TRUE,
            footer = shinyWidgets::dropdownButton(
                size = "sm", icon = icon("code"), width = "500px",
                tooltip = shinyWidgets::tooltipOptions(
                    title = "Click to see code"),
                label = "see code",
                shinyAce::aceEditor(
                    ns("code-chunk"), mode = "r", readOnly = TRUE,
                    fontSize = 14, value =
                        glue('
                    clearableTextInput(ns("text1"), "text input example"),
                    strong("Simple action button example"), br(),
                    actionButton(
                        ns("op1"),
                        label = "click me",
                        icon("cog")
                    ),
                    shinyWidgets::radioGroupButtons(
                        inputId = ns("radio_gp1"),
                        label = "radio group example",
                        choices = c("A",
                                    "B", "C", "D"),
                        status = "primary",
                        checkIcon = list(
                            yes = icon("ok", lib = "glyphicon"),
                            no = icon("remove", lib = "glyphicon"))
                    ),
                    shinyWidgets::sliderTextInput(
                        inputId = ns("slider1"),
                        label = "slider example",
                        choices = c(1, 10, 100, 500, 1000),
                        grid = TRUE
                    )
                    ')
                )
            ),
            p("You need to write your own UI and server logic"),
            p("This section is just for demo and will not be included when you
              create a new tab"),
            clearableTextInput(ns("text1"), "text input example"),
            strong("Simple action button example"), br(),
            actionButton(
                ns("op1"),
                label = "click me",
                icon("cog")
            ),
            shinyWidgets::radioGroupButtons(
                inputId = ns("radio_gp1"),
                label = "radio group example",
                choices = c("A",
                            "B", "C", "D"),
                status = "primary",
                checkIcon = list(
                    yes = icon("ok", lib = "glyphicon"),
                    no = icon("remove", lib = "glyphicon"))
            ),
            shinyWidgets::sliderTextInput(
                inputId = ns("slider1"),
                label = "slider example",
                choices = c(1, 10, 100, 500, 1000),
                grid = TRUE
            )
        )
    )
}

#' hr line with color #3b8dbc38
#' @export
#' @return HTML elements
#' @examples
#' spsHr()
spsHr <- function() {
    hr(style ='border: 0.5px solid #3b8dbc38;')
}

#' @rdname pgPaneUpdate
#' @importFrom shinydashboardPlus timelineBlock timelineLabel
#' @importFrom shinyWidgets progressBar
#' @param titles labels to display for each progress, must have the same length
#' as `pg_ids`
#' @param pg_ids a character vector of IDs for each progress. Don't forget
#' to use `ns` wrap each ID.
#' @param title_main If not specified and pane_id contains 'plot', title will be
#' 'Plot Prepare'; has 'df' will be 'Data Prepare', if neither will be
#' "Progress"
#' @param opened bool, if this panel is opened at start
#' @export
pgPaneUI <- function(pane_id,  titles, pg_ids, title_main=NULL, opened = FALSE){
    if(is.null(title_main)) {
        title_main <- pane_id %>% {
            if(str_detect(., "plot")) "Plot Prepare"
            else if(str_detect(., "df")) "Data Prepare"
            else("Progress")
        }
    }
    assert_that(is.character(titles))
    assert_that(is.character(pg_ids))
    assert_that(length(titles) == length(pg_ids))

    lapply(seq_along(pg_ids), function(i) {
        tags$li(style = "margin-bottom: 0;",
                tags$i(id = glue("{pg_ids[i]}-icon"),
                       class = "fa fa-times bg-red"),
                div(class = "timeline-item",
                    h3(class = "timeline-header no-border", titles[i]),
                    div(class="timeline-body", style = "padding: 0px;",
                        shinyWidgets::progressBar(
                            glue("{pg_ids[i]}-pg"), striped = TRUE,
                            status = "primary", 0
                        )
                    )
                )
        )
    }) %>% {
        shinydashboardPlus::timelineBlock(reversed = FALSE,
                                          id = glue("{pane_id}-timeline"),
                      .,
                      shinydashboardPlus::timelineLabel(
                          id = glue("{pane_id}-pg-label"),
                          "Ready",
                          color = "orange"),
                      div(style = "margin-left: 60px; margin-right: 15px;",
                          shinyWidgets::progressBar(
                              glue("{pane_id}-pg-all"), striped = TRUE,
                              status = "primary", 0
                          )
                      )
        )
    } %>% {
        div(class = "tab-pane", id = glue("{pane_id}-pg-container"),
            absolutePanel(
                top = "3%", right = "2%", draggable = TRUE, width = "310",
                height = "auto", class = "control-panel", cursor = "inherit",
                style = "background-color: white; z-index:999;",
                fluidRow(
                    column(3),
                    column(7, h4(title_main)),
                    column(2,
                           HTML(glue('<button class="action-button ',
                                     'bttn bttn-simple bttn-xs bttn-primary ',
                                     'bttn-no-outline"',
                                     'data-target="#{pane_id}-pg-collapse"',
                                     ' data-toggle="collapse">',
                                     '<i class="fa fa-minus"></i></button>')))
                ),
                div(class = if(opened) "collapse in" else "collapse",
                    id = glue("{pane_id}-pg-collapse"), .)
            )
        )
    }
}


#' @rdname hexPanel
#' @param hex_img single value of `hex_imgs`
#' @param hex_link single value of `hex_links`
#' @param footer single value of `footers`
#' @param footer_link single value of `footer_links`
#' @param x character of number, X offset, e.g. "-10" instead of -10L
#' @param y character of number, Y offset
#' @importFrom shinyAce is.empty
#' @export
hexLogo <- function(id, title="", hex_img, hex_link = "" ,
                    footer = "", footer_link= "", x="-10", y="-20"){
    title_text <- if(shinyAce::is.empty(title)) ''
    else glue('<span class="text-info">{title}</span><br>')
    hex <-  if(shinyAce::is.empty(hex_link)) {
        glue('<polygon points="50 1 95 25 95 75 50 99 5 75 5 25"',
             'fill="url(#{id}-hex)" stroke="var(--primary)"',
             'stroke-width="2"/>')
    } else {
        glue('<a href="{hex_link}" target="_blank">',
             '<polygon class="hex" points="50 1 95 25 95 75 50 99 5 75 5 25"',
             'fill="url(#{id}-hex)" stroke="var(--primary)"',
             'stroke-width="2"/></a>')
    }
    footer_text <- if(shinyAce::is.empty(footer)) ''
    else glue('<text x=10 y=115><a class="powerby-link"',
              'href="{footer_link}" target="_blank">{footer}</a></text>')
    HTML(glue('
    <div id="{id}" class="hex-container">
      {title_text}
      <svg class="hex-box" viewBox="0 0 100 115" version="1.1" xmlns="http://www.w3.org/2000/svg">
        <defs>
          <pattern id="{id}-hex" patternUnits="userSpaceOnUse" height="100%" width="100%">
            <image href="{hex_img}" x="{x}" y="{y}" height="125%" width="125%" />
          </pattern>
        </defs>
        {hex}
        {footer_text}
      </svg>
    </div>
     '))
}


#' Hexagon logo and logo panel
#' @description Shiny UI widgets to generate hexagon logo(s).
#' `hexLogo()` generates a single hexagon, and `hexPanel()`
#' generates a panel of hex logos
#' @param id input ID
#' @param title title of the logo, display on top of logo or title of logo panel
#' displayed on the left
#' @param hex_imgs a character vector of logo image source, can be online or
#' local, see details
#' @param hex_links a character vector of links attached to each logo, if not
#' `NULL`, must be the same length as `hex_imgs`
#' @param hex_titles similar to `hex_links`, titles of each logo
#' @param footers a character vector of footer attached to each logo
#' @param footer_links a character vector of footer links, if not `NULL`,
#' must be the same length as `footers`
#' @param xs a character vector X coordinate offset value for each logo image,
#' default -10
#' @param ys Y coordinates offset, must be the same length as `xs`, default -20
#' @details
#' The image in each hexagon is resized to the same size as the hex border
#' and then enlarged 125%. You may want to use x, y offset value to change
#' the image position.
#'
#' If your image source is local, you need to add your local directory to the
#' shiny server, e.g. `addResourcePath("sps", "www")`. This example add `www`
#' folder under my current working directory as `sps` to the server. Then you
#' can access my images by `hex_imgs = "sps/my_img.png"`.
#'
#' some args in `hexPanel` are character vectors, use `NULL` for the default
#' value. If you want to change value but not all of your logos, use `""` to
#' occupy space in the vector. e.g. I have 3 logos, but I only want to add
#' 2 footer and only 1 footer has a link:
#' `footers = c("footer1", "footer2", "")`,
#' `footer_links = c("", "https://mylink", "")`. By doing so  `footers` and
#' `footer_links` has the same required length.
#'
#' [useSps()] must be called on Shiny UI to load the required css style.
#' @export
#' @return HTML elements
#' @examples
#' if(interactive()){
#'     ui <- fluidPage(
#'         useSps(),
#'         hexPanel(
#'             "demo1", "DEMO 1:" ,
#'             rep("https://live.staticflickr.com/7875/46106952034_954b8775fa_b.jpg", 2)
#'         ),
#'         hexPanel(
#'             "demo2", "DEMO 2:" ,
#'             rep("https://live.staticflickr.com/7875/46106952034_954b8775fa_b.jpg", 2),
#'             rep("https://www.google.com", 2),
#'                  c("hex1", "hex2")
#'         ),
#'         hexPanel(
#'             "demo3", "DEMO 3:" ,
#'             rep("https://live.staticflickr.com/7875/46106952034_954b8775fa_b.jpg", 2),
#'             footers = c("hex1", "hex2"),
#'             footer_links = rep("https://www.google.com", 2)
#'         )
#'     )
#'     server <- function(input, output, session) {
#'     }
#'     shinyApp(ui, server)
#' }
hexPanel <- function(id, title, hex_imgs, hex_links=NULL, hex_titles = NULL,
                     footers=NULL, footer_links=NULL, xs=NULL, ys=NULL){
    if(not_empty(hex_titles))
        assert_that(length(hex_titles) == length(hex_imgs))
    if(not_empty(hex_links))
        assert_that(length(hex_imgs) == length(hex_links))
    if(not_empty(footers)){
        assert_that(length(footers) <= length(hex_imgs))
        assert_that(length(footers) == length(footer_links))
    }
    if(not_empty(xs))
        assert_that(length(hex_imgs) == length(xs))
    if(not_empty(ys))
        assert_that(length(hex_imgs) == length(ys))
    if(is.null(xs)) xs <- rep("-10", length(hex_imgs))
    if(is.null(ys)) ys <- rep("-20", length(hex_imgs))
    lapply(seq_along(hex_imgs), function(i){
        div(class="hex-item",
            hexLogo(id = paste0(id, i), title = hex_titles[i],
                    hex_img = hex_imgs[i], hex_link = hex_links[i],
                    footer = footers[i], footer_link = footer_links[i],
                    x = xs[i], y=ys[i])
        )
    }) %>% {
        fluidRow(class = "hex-panel",
                 h5(class = "text-primary", title),
                 tagList(.)
        )
    }
}

#' h2 title with bootstrap info color
#'
#' @param title title text
#' @param ... other attributes and children to this element
#' @return a h2 level heading with bootstrap4 "info" color(bt4 color not the
#' default bt3 info color)
#' @export
#'
#' @examples
#' tabTitle("This title")
tabTitle <- function(title, ...){
    h2(title, style = "color:#17a2b8;", ...)
}



#' Bootstrap popover trigger on hover instead of click
#' @description enhanced Bootstrap 3 popover by hovering, see
#' [bsplus::bs_embed_popover] for details. Everything is the same but has
#' additional trigger method, default "hover"
#' @param tag [htmltools::tag], generally htmltools::tags$button() or htmltools::tags$a(), into which
#' to embed the popover
#' @param title character, title for the popover, generally text
#' @param content character, content for the popover body, can be HTML
#' @param placement character, placement of the popover with respect to `tag`
#' @param trigger trigger method, default "hover", one of click | hover |
#' focus | manual.
#' @param ... other named arguments, passed to [bsplus::bs_set_data()]
#' @importFrom bsplus bs_embed_popover
#' @return shiny element
#' @export
#'
#' @examples
#' if(interactive()){
#'     library(shiny)
#'     ui <- fluidPage(
#'         useSps(),
#'         actionButton('a', 'a') %>%
#'             bsHoverPopover(title = "title a", content = "a", placement = "bottom"),
#'         tags$a("b") %>%
#'             bsHoverPopover(title = "title b", content = "b", placement = "bottom")
#'     )
#'     server <- function(input, output, session) {}
#'     shinyApp(ui, server)
#' }
bsHoverPopover <- function(
    tag, title = NULL, content = NULL, placement = "top", trigger="hover", ...){
    bsplus::bs_embed_popover(
        tag, title = title, content = content, placement = placement, ...) %>%{
            if(trigger == "hover")
                tagAppendAttributes(., `pop-toggle` = trigger)
            else tagAppendAttributes(., `data-trigger` = trigger)
        }
}



# credits to Abhi Sharma @ https://codepen.io/abhisharma2/pen/vEKWVo/

#' A shiny timeline component
#' @description This time time is horizontal, use **spsTimeline** to define it and
#' use **updateSpsTimeline** on server to update it.
#' @param id html ID of the timeline if you are using shiny modules:
#' use namespace function to create the ID but DO NOT use namespace function
#' on server.
#' @param up_labels a vector of strings, text you want to display on top of each
#' timeline item, usually like year number. If you do not want any text for a
#' certain items, use `""` to occupy the space.
#' @param down_labels a vector of strings, text you want to display at the
#' bottom of each timeline item.
#' @param icons a list of icon objects. If you do not want an icon for certain
#' items, use `div()` to occupy the space.
#' @param completes a vector of TRUE or FALSE, indicating if the items are
#' completed or not. Completed items will become green.
#' @return returns a shiny component
#' @export
#' @details `up_labels`, `down_labels`, `icons`, `completes` must have the same
#' length.
#' @examples
#' if(interactive()){
#'     ui <- fluidPage(
#'         useSps(),
#'         column(6,
#'                spsTimeline(
#'                    "b",
#'                    up_labels = c("2000", "2001"),
#'                    down_labels = c("step 1", "step 2"),
#'                    icons = list(icon("table"), icon("gear")),
#'                    completes = c(FALSE, TRUE)
#'                )
#'          ),
#'         column(6,
#'                actionButton("a", "complete step 1"),
#'                actionButton("c", "uncomplete step 1"))
#'
#'     )
#'
#'     server <- function(input, output, session) {
#'         observeEvent(input$a, {
#'             updateSpsTimeline(session, "b", 1)
#'         })
#'         observeEvent(input$c, {
#'             updateSpsTimeline(session, "b", 1, complete = FALSE)
#'         })
#'     }
#'
#'     shinyApp(ui, server)
#' }
spsTimeline <- function(
    id, up_labels, down_labels,
    icons, completes){
    assert_that(is.character(up_labels))
    assert_that(is.character(down_labels))
    assert_that(is.list(icons))
    assert_that(length(up_labels) == length(down_labels))
    assert_that(length(up_labels) == length(icons))
    assert_that(length(up_labels) == length(completes))
    lapply(seq_along(up_labels), function(i){
        tags$li(
            class = if(completes[i]) "li complete" else "li",
            id = paste0(id, "-", i),
            div(
                class = "sps-timestamp",
                span(up_labels[i])
            ),
            div(
                class = "timeline-i",
                icons[[i]]
            ),
            div(
                class = "status",
                h4(down_labels[i])
            )
        )
    }) %>% {
        tags$ul(
            class = "sps-timeline", id = id,
            style = "white-space: nowrap;",
            .
        )
    }
}

#' @rdname spsTimeline
#' @param session current shiny session
#' @param item_no integer, which item number counting from left to right you
#' want to update
#' @param complete bool, is this item completed or not
#' @export
updateSpsTimeline <- function(session, id, item_no, complete = TRUE){
    assert_that(is.numeric(item_no))
    assert_that(is.character(id))
    assert_that(length(id) == 1)
    assert_that(item_no > 0)
    item_no <- as.integer(item_no)
    if (inherits(session, "session_proxy")) {
            id <- session$ns(id)
    }
    if(complete){
        shinyjs::addCssClass(
            paste0(id, "-", item_no),
            class = "complete",
            asis = TRUE)
    } else {
        shinyjs::removeCssClass(
            paste0(id, "-", item_no),
            class = "complete",
            asis = TRUE)
    }
}

# internal css loader unit
spsLoader <- function(id=NULL){
    if(is.null(id)) id = paste0("loader", sample(1000000, 1))
    tagList(
        singleton(
            tags$style('
            .sps-loader {
              height: auto;
              display: inline-block;
              align-items: center;
              justify-content: center;
            }
            .sps-loader .container {
              width: 80px;
              height: 60px;
              text-align: center;
              font-size: 10px;
            }
            .sps-loader .container .boxLoading {
              background-color: #3c8dbc;
              height: 100%;
              width: 6px;
              display: inline-block;
              -webkit-animation: sps-loading 1.2s infinite ease-in-out;
              animation: sps-loading 1.2s infinite ease-in-out;
            }
            .sps-loader .container .boxLoading2 {
              -webkit-animation-delay: -1.1s;
              animation-delay: -1.1s;
            }
            .sps-loader .container .boxLoading3 {
              -webkit-animation-delay: -1s;
              animation-delay: -1s;
            }
            .sps-loader .container .boxLoading4 {
              -webkit-animation-delay: -0.9s;
              animation-delay: -0.9s;
            }
            .sps-loader .container .boxLoading5 {
              -webkit-animation-delay: -0.8s;
              animation-delay: -0.8s;
            }

            @-webkit-keyframes sps-loading {
              0%,
              40%,
              100% {
                -webkit-transform: scaleY(0.4);
              }
              20% {
                -webkit-transform: scaleY(1);
              }
            }
            @keyframes sps-loading {
              0%,
              40%,
              100% {
                transform: scaleY(0.4);
                -webkit-transform: scaleY(0.4);
              }
              20% {
                transform: scaleY(1);
                -webkit-transform: scaleY(1);
              }
            }
          ')
        ),
        tags$div(
            id = id, class = "sps-loader",
            HTML('
            <div class="container">
                <div class="boxLoading boxLoading1"></div>
                <div class="boxLoading boxLoading2"></div>
                <div class="boxLoading boxLoading3"></div>
                <div class="boxLoading boxLoading4"></div>
                <div class="boxLoading boxLoading5"></div>
            </div>
           ')
        )
    )
}


#' Match height of one element to the other element
#' @description Match the height of one element to the second element.
#' If the height of second element change, the height of first element will change
#' automatically
#' @param div1 element ID, or jquery selector if `isID = FALSE`. The first element
#' that you want to match the height to the other element
#' @param div2 matched element ID or selector, the other element
#' @param isID bool, if `TRUE`, `div1` and `div2` will be treated as ID, otherwise
#' you can use complex jquery selector
#'
#' @return will be run as javascript
#' @export
#'
#' @examples
#' if(interactive()){
#'     library(shiny)
#'
#'     ui <- fluidPage(
#'         useSps(),
#'         column(
#'             3, id = "a",
#'             style = "border: 1px black solid",
#'             p("a")
#'         ),
#'         shinyjqui::jqui_resizable(column(
#'             6, id ="b",
#'             style = "border: 1px black solid",
#'             p("b")
#'         )),
#'         heightMatcher("a", "b")
#'     )
#'
#'     server <- function(input, output, session) {
#'
#'     }
#'     # Try to drag `b` from bottom right corner and see what happens to `a`
#'     shinyApp(ui, server)
#' }
heightMatcher <- function(div1, div2, isID=TRUE){
    if(isID) {
        div1 <- paste0("#", div1)
        div2 <- paste0("#", div2)
    }
    tags$script(paste0(
        'heightMatcher("', div1, '",', ' "', div2, '")'
    ))
}


