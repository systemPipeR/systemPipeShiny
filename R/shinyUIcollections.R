################## A Collections of HTML components#############################

## use on top of shiny



# input with a "X" button in the end to clear the entire typed text
# work the same as Textinput
clearableTextInput <- function(inputId, label, value = "", placeholder = "input text"){
    tagList(tags$div(
        tags$label(label, `for` = inputId),
        tags$span(
            class = "text-input-clearable",
            tags$input(id = inputId, type = "text", value = value, placeholder = placeholder),
            HTML('<span id="clear_input" class="glyphicon glyphicon-remove"></span>')
            )
        ),
        tags$style(HTML(
        "
            .text-input-clearable {
              width: 100%;
              border:1px solid #ccc;
              border-radius: 3px;
              padding:1px 6px 1px 1px;
              display:inline-block;
            }
            .text-input-clearable input {
              width: 93%;
              border:none;
              background:none;
              outline:none;
              padding:0 0;
              margin:0 0;
              font:inherit;
            }
            .text-input-clearable span {
              float: right;
              height: 14px;
              font-size: 14px;
              cursor: pointer;
              color: #ccc;
              visibility:hidden;
            }
            .text-input-focused {
              border-color: #66afe9;
              outline: 0;
              -webkit-box-shadow: inset 0 1px 1px rgba(0,0,0,.075), 0 0 8px rgba(102, 175, 233, 1);
              box-shadow: inset 0 1px 1px rgba(0,0,0,.075), 0 0 8px rgba(102, 175, 233, 1);
            }
        "
        )),
        singleton(tags$script(HTML(
            sprintf('
                var textInput = document.getElementById("%s");
                var clearBtn = textInput.nextElementSibling;
                var textInputBox = textInput.parentElement;
                textInput.onkeyup = function() {
                  clearBtn.style.visibility = (this.value.length) ? "visible" : "hidden";
                };
                clearBtn.onclick = function() {
                  this.style.visibility = "hidden";
                  textInput.value = "";
                  Shiny.setInputValue("%s", "");
                };
                textInput.addEventListener("focus", function() {
                  textInputBox.classList.add("text-input-focused");
                }, false);
                textInput.addEventListener("blur", function() {
                  textInputBox.classList.remove("text-input-focused");
                }, false);
                ',inputId, inputId)
            ))
        )
    )
}

## not very compatible with bs4, not in use
# TextInputClear <- function(inputId, label = "", placeholder = "some text", value = "./") {
#     tags$div(HTML(glue('
#     <script src="https://code.jquery.com/jquery-latest.min.js"></script>
#     <script src="https://stackpath.bootstrapcdn.com/bootstrap/4.4.1/js/bootstrap.min.js"></script>
#     <script>
#         $(document).ready(function() {
#           $("#@{inputId}@").keyup(function() {
#              $("#clear_input").toggle(Boolean($(this).val()));
#              });
#         $("#clear_input").toggle(Boolean($("#@{inputId}@").val()));
#         $("#clear_input").click(function() {
#             $("#@{inputId}@").val("").focus();
#             Shiny.setInputValue("@{inputId}@", "");
#             $(this).hide();
#         });
#         });
#     </script>
#    
#     
#     <style>
#         #@{inputId}@ {
#             width: 200px;   
#         }
#         #clear_input {
#             position: absolute;
#             right: 5px;
#             top: 0;
#             bottom: 0;
#             height: 14px;
#             margin: auto;
#             font-size: 14px;
#             cursor: pointer;
#             color: #ccc;
#         }
#     </style>
#     
#     <label class="control-label" for="@{inputId}@">@{label}@</label>
#     <div class="btn-group">
#         <input id="@{inputId}@" type="search" class="form-control" placeholder="@{placeholder}@" value="@{value}@">
#         <span id="clear_input" class="glyphicon glyphicon-remove"></span>
#     </div>
#          ', .open = "@{", .close = "}@")
#         )
#     )
# } 

