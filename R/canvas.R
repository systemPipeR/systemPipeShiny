
#wrapper for toCanvasBtn that can only be used in SPS

#' Screenshot a plot or UI to SPS Canvas or download as an image
#' @description A upper level function of [drawer::toCanvasBtn]. You should only
#' use it under SPS projects. For you own apps, still use the [drawer::toCanvasBtn].
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
    div(
        class = class,
        toCanvasBtn(dom, canvasID = "core_canvas", id=id, isID=isID)
    ) %>%
        bsHoverPopover(
            "Send to Canvas",
            "Take a screenshot of current plot and send to SPS Canvas tab,
            or save as an image",
            placement = placement
        )
}

