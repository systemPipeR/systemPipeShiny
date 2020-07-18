# ####### UI
# DO NOT delete the next line
# last change date: 20200713114337
# valid colors:
# red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black

# shiny app auto-source ./R folder only when sourcing `global.R` is done. Since
# all tab files are in ./R, has to put the app main function here. Otherwise tabs
# can't be found.
sps_app <<- sps(
    vstabs = c(
        'df_targets', 'df_count', 'plot_pca',
        'plot_box', 'plot_tsne', 'plot_mds',
        'plot_glm', 'plot_heat', 'plot_clust',
        'plot_volcano', 'df_degcount', 'df_edgeR'
    ),
    server_expr = {
        msg("Hello World", "GREETING", "green")
    }
)

sps_app$ui
