## Internal quick testing app for admin page

# addResourcePath("sps", "www")

# for (i in 1:10){
#     db$accAdd(paste0("user", i), "dsads!dddddd")
# }

#
# shinyApp(
#     div(
#         tags$head(
#           shinytoastr::useToastr(),
#         ),
#         adminUI()
#     ),
#     function(input, output, session) {
#         shared <- reactiveValues()
#         admin_infoServer("admin-info", shared)
#         admin_usersServer("admin-users", shared)
#     }
# )
