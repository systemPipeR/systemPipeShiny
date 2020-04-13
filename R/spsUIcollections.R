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
