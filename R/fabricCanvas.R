# funcs --------------------------
toCanvasBtn <- function(dom, canvasID, id="", label = "To canvas", color_class = "primary"){
    div(
        class="btn-group to-canvas",
        id = id,
        tags$button(
            type="button",
            class=paste0("btn btn-", color_class),
            onclick=paste0('toCanvas("', dom, '", "', canvasID, '");
                           if(toastr) toastr.info("Sceenshot sent to Canvas", "", {positionClass: "toast-bottom-right", timeOut: 2000});
                           '),
            label
        ),
        tags$button(
            type="button",
            class=paste0("btn btn-", color_class, " dropdown-toggle"),
            `data-toggle`="dropdown",
            `aria-haspopup`="true",
            `aria-expanded`="false",
            tags$span(class="caret"),
            tags$span(class="sr-only", 'Toggle Dropdown')
        ),
        tags$ul(
            class = "dropdown-menu",
            tags$li(tags$a(href = "#", "Save as png", onclick = paste0('toPng("', dom, '")'))),
            tags$li(tags$a(href = "#", "Save as jpg", onclick = paste0('toJpg("', dom, '")')))
            # tags$li(tags$a(href = "#", "Save as svg", onclick = paste0('toSvg("', dom, '")')))
        )
    )
}

#wrapper for toCanvasBtn that can only be used in SPS

#' Screenshot a plot or UI to SPS Canvas or download as an image
#'
#' @param dom a HTML DOM selector, mostly common is to select the element by ID:
#'
#' e.g. a plot with ID "plot1", to select, use `dom = "plot1"` to select the plot if
#' `isID = TRUE`. If `isID = FALSE`, use `dom = "#plot1"`
#'
#' Other complex selector is supported. First turn `isID = FALSE`, then try things
#' like `dom = ".btn i"` selects an icon inside an element
#' with "btn" class. If more than one element is matched, only the first one will
#' be screenshoted.
#' @param id ID of this button, optional.
#' @param isID bool, if the `dom` argument is selected by ID or other selector
#' @param class string, length 1, other html class add to the button wrapper
#' @param placement where should the tiptool place, top, bottom, left, right.
#' @return a button group with several options
#' @export
#'
#' @examples
#' canvasBtn("#mydiv")
canvasBtn <- function(dom, id = "", isID = TRUE, class = "text-center",
                      placement = "top"){
    dom = if(isID) paste0("#", dom) else dom
    div(
        class = class,
        toCanvasBtn(dom, canvasID = "core_canvas", id=id)
    ) %>%
        bsHoverPopover(
            "Send to Canvas",
            "Take a screenshot of current plot and send to SPS Canvas tab,
            or save as an image",
            placement = placement
        )
}

bsModal <- function(id, ..., title="title",
                    size=c('normal', 'large', 'small'),
                    confirmbtn = FALSE,
                    confirmbtn_id = paste0(id, "-confirm"),
                    confirmbtn_text = "confirm"
){
    size <- switch(size[1],
                   'large' = 'modal-lg',
                   'small' = 'modal-sm',
                   ''
    )
    div(
        class="modal fade", id= paste0(id, "-modal"), tabindex="-1", role="dialog",
        `aria-labelledby`=paste0(id, "-modal-title"),
        div(
            class=paste("modal-dialog", size), role="document",
            div(
                class="modal-content",
                div(
                    class="modal-header",
                    HTML('<button type="button" class="close" data-dismiss="modal" aria-label="Close">
                  <span aria-hidden="true">&times;</span>
                </button>
          '),
                    h4(class="modal-title", id=paste0(id, "-modal-title"), title)
                ),
                div(
                    class="modal-body", ...
                ),
                div(
                    class="modal-footer",
                    tags$button(type="button", class="btn btn-default", `data-dismiss`="modal", "Close"),
                    if(confirmbtn){actionButton(inputId = confirmbtn_id, label = confirmbtn_text, class="btn-primary")} else {div()}
                )
            )
        )
    )
}

bannerBtn <- function(id, icon, onclick = "",
                      tip = "", `data-placement` = "bottom",
                      `data-toggle` = "tooltip", div_style = "",
                      label = "", btn_style = "",
                      ...){
    div(
        class = "banner-items",
        style = div_style,
        actionButton(
            id, label = label, class = "item", icon = icon,
            onclick = onclick,
            `data-toggle` = `data-toggle`,
            `data-placement` = `data-placement`,
            title = tip,
            style = btn_style,
            ...
        )
    )
}

canvas = function(canvasID, title = "Canvas", height = "85vh", width = "100%",
                  on_start = TRUE){
    tagList(
        tags$head(
            tags$script(src = "sps/js/dom-to-image.min.js"),
            tags$script(src = "sps/js/spectrum2.0.5.js"),
            tags$script(src = "sps/js/fabric.min.js"),
            tags$script(src = "sps/js/fabric-history.js"),
            tags$script(src = "sps/js/split1.6.0.js"),
            tags$script(src = "sps/js/FileSaver.js"),
            tags$link(href="sps/css/dom-to-canvas.css", rel="stylesheet"),
            tags$link(href="sps/css/spectrum2.0.5.css", rel="stylesheet"),
            tags$script(src = "sps/js/dom-to-canvas.js")
        ),
        div(
            class = "canvas-box",
            style = paste0(
                "height: ", height, ";\n",
                "width: ", width, ";\n"
            ),
            div(
                id = paste0("canvas-banner-", canvasID),
                class = "canvas-banner",
                div(
                    class = "banner-items",
                    tags$span(title)
                ),
                div(class = "vr"),
                #<li role="separator"  class="divider"></li>
                div(
                    class = "banner-items",
                    HTML('
        <div class="dropdown item">
          <button class="btn btn-default dropdown-toggle" type="button" id="drop-file" data-toggle="dropdown" aria-haspopup="true" aria-expanded="true">
            File
            <span class="caret"></span>
          </button>
          <ul class="dropdown-menu" aria-labelledby="drop-file">
            <li class="dropdown-submenu">
              <div class="check-box"></div>
              <a href="#">Save as...</a>
              <ul class="dropdown-menu">
               <li ><a href="#"', paste0('onclick=DTC["', canvasID, '"].saveAsImg()'), '>png</a></li>
               <li ><a href="#"', paste0('onclick=DTC["', canvasID, '"].saveAsImg(false)'), '>jpg</a></li>
               <li ><a href="#"', paste0('onclick=DTC["', canvasID, '"].saveAsSvg()'), '>svg (coming soon...)</a></li>
              </ul>
            </li>
          </ul>
        </div>
        ')
                ),
                div(
                    class = "banner-items",
                    HTML('
        <div class="dropdown item">
          <button class="btn btn-default dropdown-toggle" type="button" id="dropdownMenu1" data-toggle="dropdown" aria-haspopup="true" aria-expanded="true">
            View
            <span class="caret"></span>
          </button>
          <ul class="dropdown-menu dropdown-state" aria-labelledby="dropdownMenu1">
            <li check=true opt-id="grid"><div class="check-box"><i class="fa fa-check"></i></div><a href="#">Grid line</a></li>
            <li check=true opt-id="frame" ><div class="check-box"><i class="fa fa-check"></i></div><a href="#">Frame box</a></li>
            <li check=true opt-id="boundBox" ><div class="check-box"><i class="fa fa-check"></i></div><a href="#">Object bound box</a></li>
            <li check=true opt-id="objInfo" ><div class="check-box"><i class="fa fa-check"></i></div><a href="#">Object info on select</a></li>
          </ul>
        </div>
        ')#<li role="separator"  class="divider"></li>
                ),
                div(
                    class = "banner-items",
                    HTML('
        <div class="dropdown item">
          <button class="btn btn-default dropdown-toggle" type="button" id="dropdownMenu1" data-toggle="dropdown" aria-haspopup="true" aria-expanded="true">
            Canvas
            <span class="caret"></span>
          </button>
          <ul class="dropdown-menu dropdown-state" aria-labelledby="dropdownMenu1" style="min-width: 170px;">
            <li check=true opt-id="limitMove" ><div class="check-box"><i class="fa fa-check"></i></div><a href="#">Limit objects inside grid</a></li>
          </ul>
        </div>
        ')
                ),
                div(
                    class = "banner-items",
                    HTML('
        <div class="dropdown item">
          <button class="btn btn-default dropdown-toggle" type="button" id="dropdownMenu1" data-toggle="dropdown" aria-haspopup="true" aria-expanded="true">
            Help
            <span class="caret"></span>
          </button>
          <ul class="dropdown-menu" aria-labelledby="dropdownMenu1">
            <li ><div class="check-box"></div><a href="#" type="button" data-toggle="modal"', paste0('data-target="#', canvasID, "-shortcut-modal\""), '>Keyboard shortcuts</a></li>
            <li ><div class="check-box"></div><a href="#" type="button" data-toggle="modal"', paste0('data-target="#', canvasID, "-help-modal\""), '>About the canvas</a></li>
          </ul>
        </div>
        ')#<li role="separator"  class="divider"></li>
                ),
                div(class = "vr"),
                bannerBtn(
                    paste0("canvas-undo-", canvasID),
                    tags$i(tags$img(src = "sps/img/undo.png", width=15, height=15)),
                    tip = "undo Ctrl+z",
                    onclick = paste0('DTC["', canvasID, '"].undo()'),
                    disabled = ""
                ),
                bannerBtn(
                    paste0("canvas-redo-", canvasID),
                    tags$i(tags$img(src = "sps/img/redo.png", width=15, height=15)),
                    tip = "redo Ctrl+shift+z",
                    onclick = paste0('DTC["', canvasID, '"].redo()'),
                    disabled = ""
                ),
                bannerBtn(
                    paste0("canvas-front-", canvasID),
                    tags$i(tags$img(src = "sps/img/tofront.png", width=15, height=15)),
                    tip = "bring to front",
                    onclick = paste0('DTC["', canvasID, '"].toFront()')
                ),
                bannerBtn(
                    paste0("canvas-forward-", canvasID),
                    tags$i(tags$img(src = "sps/img/forward.png", width=15, height=15)),
                    tip = "move forward",
                    onclick = paste0('DTC["', canvasID, '"].toForward()')
                ),
                bannerBtn(
                    paste0("canvas-backward-", canvasID),
                    tags$i(tags$img(src = "sps/img/backward.png", width=15, height=15)),
                    tip = "move backward",
                    onclick = paste0('DTC["', canvasID, '"].toBackward()')
                ),
                bannerBtn(
                    paste0("canvas-back-", canvasID),
                    tags$i(tags$img(src = "sps/img/toback.png", width=15, height=15)),
                    tip = "bring to back",
                    onclick = paste0('DTC["', canvasID, '"].toBack()')
                ),
                bannerBtn(
                    paste0("canvas-del-", canvasID),
                    icon("trash"),
                    tip = "delete (del)",
                    onclick = paste0('DTC["', canvasID, '"].delItem()')
                ),
                div(class = "vr"),
                div(
                    class = "banner-items",
                    tags$select(
                        id = paste0("canvas-opacity-", canvasID),
                        class = "form-control",
                        `data-toggle` = 'tooltip',
                        `data-placement` = 'left',
                        title = "object opacity",
                        onclick = paste0('DTC["', canvasID, '"].changeOpacity($("#canvas-opacity-', canvasID , '").val())'),
                        tags$option("100%", value = 1),
                        tags$option("90%", value = 0.9),
                        tags$option("80%", value = 0.8),
                        tags$option("70%", value = 0.7),
                        tags$option("60%", value = 0.6),
                        tags$option("50%", value = 0.5),
                        tags$option("40%", value = 0.4),
                        tags$option("30%", value = 0.3),
                        tags$option("20%", value = 0.2),
                        tags$option("10%", value = 0.1),
                        tags$option("0%", value = 0)
                    )
                )
            ),
            div(
                class = "canvas-body",
                div(
                    id = paste0("canvas-left-", canvasID),
                    class ="split split-horizontal canvas-left",
                    bsplus::bs_accordion(id = paste0("tool-box-", canvasID)) %>%
                        bsplus::bs_set_opts(panel_type = "info") %>%
                        bsplus::bs_append(
                            "Images/ Plots",
                            div(
                                id = paste0("img-box-", canvasID),
                                class = 'canvas-img-box',
                                tags$img(draggable="true", src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAGQAAABkCAYAAABw4pVUAAAABHNCSVQICAgIfAhkiAAADXhJREFUeJztm3lAFeXex79zzgEEFFAQJTUDVAS8by6AS5opZuZGmiXpi5gLiuKKa6ZeN9QU7WoKgiuUyc3c0NfqmmAuKOaGIqmUcGUXRfbgLN/3D456EEQOcGn0zufPc2ae88z5zPyeeZ7vjECSkBANsr+6AxLleamFaApTcDkmATmv0DUuA4DbJ0Lh69IQMnljdP5oCvz8/ODnNxWTx32E3nYWcFl1E+q/uqflUCP9wm7M6OuMbj7fIrlWnVPiXsw/8eX/OsJQJodFj+kIPXoN2X+VZJIkVUxY7UoDA1euTlBRF3VyMP3Xx7P8p2JAxYQAFxp3WMwrytq3VnpmFu3kRnTfmkpN7ZurMYoyLQIMDBQQoKogTPb6GCyeKIe8nk+U6iDI6q7iCgYGUAiAgaFBnbVZE6o+oqJoLF1+Cg3MDAEQOfHfY17PxpCb9kHAtWykHfVBJ5eJCDmTgpKyHRC/ZzamLNyE0G0rMHbwaGy5UgRAhdSzIZj/YTe4zdiLE0H+GNWnPWxa98HS6GRcCluE8UPd8EZzJ4zafQcq5iH+wBfw8+iKgX//Blt93OFk3QhN2g/BiuhsVF5NiLy4CASuXIWlM0fgrS4DsOBYSg1LrZ791e6lTjuBL+YuxsZ/LMFHzqaQKWzQY5QfQi4pq//TZReKmonru9NQZsUuH4yht7c3vb0+pruDBa3GHuWfOpeU6m4IB1pZcmDIFR5f4MOtt57WC03Gdg5o6MD5F5QklTw/z4GWow+ykCSZwz2DG7CB22c8nakkmcvD3s3ZoPVwbrtRRFLNlK19aWLvz3OlpLrgFGe2NaCNx1bG5aqpKYhnsIcNFc29eOihhqSKv61xe1Ky1Cnh9PxgA2+rSFLFW2vdaNhkBPfer14BUl6Yx3YKIw7YnqUtWfr1l5oMhnlY031LCtUklfGr6GJkyU+PldSkZGmR22LE6p1Y0L6sQDFrLyauEcpv8sY4bF6xH67+AyAEnMXhdk+bEKw/xMYfXWDRWY7CpNM4n1iIotwM5BIwEYzRsKEc8oZd4GqtAGCKdg4tIVxyhpuDMQCgaevWME5Pwr9Lge4NGqOxqQI2rr3hZCaDACeMXzYRW9yCcDRWCY/3dIuoBnf3BeNMXmd8t24NZACY7YC+XYqRkawGrMofZvXQs7+4jUtX86DsW3a9KNq+jV4tApGt1u/uoMqeCk0HYLLng2c2ksPOcxIGLPFE1OmLyJpiD5vHhU8wh43pDWxZ+TPs+g+Di6M5hBgNNAQgVGi+4hggEyBQCWXFoayss20c0VZWjMKiZw9SheTf76GR20bMW+D6pL8Lqzq4GlBlf02c8M7bVth1JBJJU/xgV5iBLLMPMPItQ71+o+oxRGgCF7e20Nz5F37+XVuNmYdTX55E183z8NqxOZhzIPNJTVff3IDB/cPRdNIsePZoBeNKJNQG5uciX+GMTs7PnkcymFmYICkqCn/oDhrqVPx6KQ2auu1G5QiW+CDoAD6zisKmwFDs+q4Inx7aiiFN9PsTtEIIZakShPZs1oF5v2Ld9DCkmssAEDk/ByDcahqmjFyEoFlNcWjWHBzKIgDiYcwJXHyUh4e5SrA4GVdvZkNdUoISpRIqEHymcZKARv104NUQZS092QJFBQXaP1SDlMhI3BvqjzFttSVVowFIEAr8bchQtL6yCqOm7UHM3Uyk34lGyPyNSDBvVq3ZL5VKKAkoSx8PwHr2lzk4sXQ5kgcvhI9HL3R/uytaaXJRpO/ZQJK3/xXMiR2NKQjGtHf3oo/vVE6d6svxowbSpYUxjXpvYrK6lPei13KIw7tcdyOPGmqYFT2LHQ1ktO77OSPvFFCdcpB+rs3YyPpNeszewR+3j2Yrszf47uJIXr6wk2PbKqhwmsiwS2l8+PtJrurfhDKTrpxz4AbvZ17lN5P+RgO5HT/Zepb3CuK4rKMRW7pP4ryFS/j3RX4cNyOUV/I0JDW8H3eQi3qbU2bWm58fvs4sdSGv7/Jhz9ZmNDSyoG2vCQz+9VE15hSl/Pe5CAZ+0o4Ggozm3acxJPIi42L17G9RDn9Z8g7btHqN5kZyCgIIQUaLrst4vrD6gzr0ugWoT1RxXNbRmJ1XinFSWhF16mHOnhrKRJ1Jqrokl4k7F3HDterPXMW9lkVtWRI9KlzftgRhv+Ugt+DpIKbK+Q0xj5zh7lD9uzxxCuFDxB/dh6jkEiT+sBsRpxKRL+oFRAWcx62Aj+leeNg3w+tObugz1BuLIh6g+6RP8D9G1W9JIKU8REyI8wr5L0YSIjIkISKj3PCfdPprhC6bjTUn82D7rhfeb2eI0rz7SEkvQlO3YRjv64VeLfRbCpDQk/J3wRpmhb5HI7k9Z58tffppXjy/9nmTZhad6Hc45aWYF9QW1R/h3Hwoq97DqgolS2FQMaARGjlhdNAxbOmbiiDvcQi9+zLMDWoOH0Rh4YjpOJJR/8F19ccQWQt8PMMTrfKjsC38AuKPbsbckb0waHkEgkc5w9LyHXx5K/85AVUZNQ9wahIY1TSw+hO3Tx3CmT8KcPf4l1j1xSHcUgNFCd9iweRZWBm4ASv8hmPQ2ECcyvwPCCt/wWj4cPfgCiXrybcPdnGQkUCTjyKYX3ias9oY0LLXUp78I4E/7N7Py3dCnx9Q1TrA0TPgqk1gVRrNaa8bs19wOjUkNVkH6GXfk+tuaYu1Jovfj27BRl0DeK3i31Qr9EpuBLkCcgEQBBnkRpawMlegRd9h6GXbHgrb9gAfYeOPrpUHVKraBjj6BEYaWNdZYKVB6ndf4XuhG3xttaGY0BSDJwxD4/7bsSd2DgLfqrscXi8hqsQEJKoUcOzojMf3WoKgs95fVUClqJsAR5fnB0Z1GVhpkJGSDlVpCUp1PlW0dYS9bBfSMut2PK3+GMJcnNy+D3fM34efl0OlT6FUGVDVUYBTPeoysJKjpW0ryDPjEJeus2dpCUrl9nBoU7fP41QQolQqgWef6yhNx6m1nhgXYYGp4SHwalkWVhHa0AbACwOqWgc4+gRGtQysBGOYGBPZWfehVt4H+kzDKJtY7NpxFX9q+5IRHYXsQfMxsUNN8vqqDlOHu7+EcX4fS8oEQ9r2m8Bp06fSx/tjDu73Hj+e/RVPJBVrB7WHvH5gPnuZy9iwmz8jYpJYSFYRUP3IDFVtApwSpl7UMzAqrmlgRZJFjA3oTRsrRw7xD+PVfLLoVgTnDO3PkbNXcf3aRZyxcA/j8vUdsl9MvQVUdRXgvOrU01pW3QU4rzr1loeokiOxdPoShJ25B6GZHezbOMKlnycmT3gf9ib10YOXAymgEhnS8rvIkISIDEmIyKj09kaVcQ47129AxK/5aNjcGo2Uabjyy2kkZGtgP+c0bq7rjr/2LYpXl2eEEHmxX2C4x2rkjPwaB38ajNe1S03MvYxNoz2wtf77+N9FuVlJ7k/0tVXQ5K1A3qokFtRkfsu5ATGs4xVnCR10hGj48JvhNBMM2fe579kV8V5yFtUs5I3ds+i74B8MCV5O70Gj+NXlQpJ/MunkZs4Y4kL3z//JvXM96Nq6Ca07TWREkpqkiqlRS9nXUkbD7osYk6kiqWRalD/d2o/hvsRi8rltk9Rk8UTATC4OCmd40Gf0HLaasa/YJF9HiJKxCxyokFlzwg9Vh0ZVvimlusW1XY3YpPdSnkwrJYtiONfRiI4LL1Kp/Z245Z1pbDed0dqlsdILKzh9R1lwVVXbqpsB7Oa6nNdVJFnCS0FBPPOKXa6y8uULADR40eO0ZW9KRWBmZzkKk86VBVEZZUEUBFM0NJXBrFM/9LQxAIyd8WY7GbIysrTL3gp08JmB/jn7EHr8EYhinDleBPfhr0H2grZljW3Q9M5GjJ0cgvNZMnSeMAFdX7G7Cx0hctg5toUR83Dn9gsyA20QtXPlRkSmtSoLoljx3RLtxhCEsqXyx18LzUZg2ifGiNz2HVIfHMcJ+UD0txBe2LbQ3Ash+2ei2c8z0bO9C8YEXxb5M7/6oyNEgOUgbwy1VuFCWBiul1S+g1qlroM3pUzQ23c82pwJxuo1MbAa1h0NHrdfRdvMSYe622Ici4/HsXlv4OzcIZhxJE/fHxc15UqWYDUcG3b4wuH2GnhOCkO87unHPNzcH4CFweeRdO5Fb0rptqqGWv24HD5F4TQOk3reRtg1e4xwfJy6VR1yldz9FluO3AeNbfHegl1Y0k+N/ALVc16TfkmpbGAp+O0gV336Ljt36MJ3Bo3gaO/xnOS3kJt/ustiVhVE/R/jzgVySHM5TXvMZ2TCA2Zc3MQPW8qp6ODL/Qm6AZGGOUfmcs7hh+Xu6KoKuVJiP2MHmx70Xb+T4TtW039+GOOL/7ODbH0jrfaKDGktS2RIQkSGJERkSEJEhiREZEhCRIYkRGRIQkSGJERkSEJEhiREZEhCRIYkRGRIQkSGJERkSEJEhiREZEhCRIYkRGRIQkSGJERkSEJEhiREZEhCRIYkRGRIQkSGJERkSEJEhiREZEhCRIYkRGRIQkSGJERkSEJEhiREZEhCRIYkRGRIQkSGJERkSEJEhiREZEhCRIYkRGRIQkSGJERkSEJEhiREZEhCRIYkRGRIQkSGJERkSEJExv8D4YKcO0kqvGMAAAAASUVORK5CYII=", width="125", height="125"),
                            )
                        ) %>%
                        bsplus::bs_append(
                            "Text",
                            div(
                                id = paste0("text-", canvasID),
                                class = 'sidebar',
                                bannerBtn(
                                    paste0("canvas-itext-", canvasID),
                                    div_style = "width: 100%",
                                    btn_style = "width: 100%",
                                    label = "New iText",
                                    icon("i-cursor"),
                                    tip = "Create new text",
                                    onclick = paste0('DTC["', canvasID, '"].newItext()')
                                ),
                                tags$hr(),
                                bannerBtn(
                                    paste0("canvas-bold-", canvasID),
                                    icon("bold"),
                                    tip = "Bold ctrl+b",
                                    onclick = paste0('DTC["', canvasID, '"].itextBold()')
                                ),
                                bannerBtn(
                                    paste0("canvas-italic-", canvasID),
                                    icon("italic"),
                                    tip = "italic ctrl+i",
                                    onclick = paste0('DTC["', canvasID, '"].itextItalic()')
                                ),
                                bannerBtn(
                                    paste0("canvas-underline-", canvasID),
                                    icon("underline"),
                                    tip = "underline",
                                    onclick = paste0('DTC["', canvasID, '"].itextUnderline()')
                                ),
                                bannerBtn(
                                    paste0("canvas-linethrough-", canvasID),
                                    icon("strikethrough"),
                                    tip = "Linethrough",
                                    onclick = paste0('DTC["', canvasID, '"].itextLinethrough()')
                                ),
                                bannerBtn(
                                    paste0("canvas-superscript-", canvasID),
                                    icon("superscript"),
                                    tip = "superscript ctrl+.",
                                    onclick = paste0('DTC["', canvasID, '"].itextSuperScript()')
                                ),
                                bannerBtn(
                                    paste0("canvas-subscript-", canvasID),
                                    icon("subscript"),
                                    tip = "subscript ctrl+,",
                                    onclick = paste0('DTC["', canvasID, '"].itextSubScript()')
                                ),
                                bannerBtn(
                                    paste0("canvas-removeformat-", canvasID),
                                    icon("remove-format"),
                                    tip = "remove script Ctrl+shift+.",
                                    onclick = paste0('DTC["', canvasID, '"].itextRemoveFormat()')
                                ),
                                tags$hr(),
                                div(
                                    class = "banner-items color-picker",
                                    tags$input(id = paste0("canvas-text-fill-", canvasID), value = "black"),
                                    tags$p("Text Fill color", style='display: inline;'),

                                ),
                                div(
                                    class = "banner-items color-picker",
                                    tags$input(id = paste0("canvas-text-bg-", canvasID), value = "white"),
                                    tags$p("Text background color", style='display: inline;'),

                                )
                            )
                        )
                ),
                div(
                    id = paste0("canvas-container-", canvasID),
                    tabindex="-1",
                    class ="split split-horizontal canvas-container",
                    tags$canvas(id=paste0("canvas-f-", canvasID), class = "canvas-canvas"),
                    tags$canvas(id=paste0("canvas-b-", canvasID), class = "canvas-canvas")
                ),
                if(isTRUE(on_start)){
                    tags$script(paste0(
                        '$(function(){',
                        'DTC.', canvasID, ' = new dtc("', canvasID, '");',
                        '});'
                    ))
                } else {
                    tags$script(paste0(
                    'var canvasInit = false;
                    $("',  on_start, '").on("click", function(){
                        if (!canvasInit){
                            setTimeout(function() {',
                        'DTC.', canvasID, ' = new dtc("', canvasID, '");
                                canvasInit = true;
                            }, 1000)
                        }
                    });
                    '
                    ))
                }
            )
        ),
        bsModal(
            id = paste0(canvasID, "-shortcut"),
            title = "Keyboard shortcuts",
            size = "large",
            div(
                id = paste0(canvasID, "-shortcut-table"),
                DT::datatable(
                    tibble::tribble(
                        ~Description, ~Shortcut,
                        'Zoom in/out', 'Mouse wheel',
                        'Panning', 'Alt+mouse hold left click and dragging',
                        'Remove object', 'del',
                        'undo', 'Ctrl+z',
                        'redo', 'Ctrl+shift+z',
                        'Select multiple objects', 'Hold shift + left clicks',
                        'bold', 'Ctrl+b',
                        'italic', 'Ctrl+i',
                        'superscript', 'Ctrl+.',
                        'subscript', 'Ctrl+,',
                        'renmove script', 'Ctrl + shift + .'
                    ),
                    width = "100%"
                )
            ),
            tags$script(paste0(
                '
      var shortcutTableShown = false;
      $("#', canvasID, '-shortcut-modal").on("show", function(){
        if(shortcutTableShown === false){
          $(".datatables.html-widget").trigger("shown");
          shortcutTableShown = true;
        }
      })
      '
            ))
        ),
        bsModal(
            id = paste0(canvasID, "-help"),
            title = "About the canvas",
            p("Canvas V0.1"),
            p("This canvas is developed by Le Zhang and other contributors under AGPL License")
        )
    )
}
