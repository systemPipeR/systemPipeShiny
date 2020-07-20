# ####### UI
# DO NOT delete the next line
# last change date: 20200713114337
# valid colors:
# red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black


sps_app <<- sps(
    vstabs = c("plot_pca", "plot_box"),
    server_expr = {
        msg("Hello World", "GREETING", "green")
    }
)

sps_app$ui
