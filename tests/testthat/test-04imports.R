########## Test import functions behave as we want

test_that("DT funcs", {
    expect_s3_class(DT::renderDT({}) , "shiny.render.function")
    df <- DT::datatable(
        iris,
        style = "bootstrap",
        class = "compact",  filter = "top",
        extensions = c( 'Scroller','Buttons'),
        options = list(
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
            deferRender = TRUE,
            scrollY = 580, scrollX = TRUE, scroller = TRUE,
            columnDefs = list(list(className = 'dt-center',
                                   targets = "_all"))
        ))
    expect_s3_class(df, "datatables")
})

test_that("DT snap", {
    skip_on_bioc()
    expect_snapshot_output(DT::DTOutput(""))
})


test_that("R6 class", {
    testR6 <- R6::R6Class(
        classname = "testR6",
        public = list(
            initialize = function(){
                message("create a new R6")
            },
            addList = function(num){
                private$my_list[[length(private$my_list) + 1]] <- num
            },
            showList = function(){
                return(private$my_list)
            }
        ),
        private = list(
            my_list = list()
        )
    )
    expect_message(testR6$new())
    newR6 <- testR6$new()
    expect_type(newR6$showList(), "list")
    newR6$addList(5)
    newR6$addList(5)
    expect_length(newR6$showList(), 2)
})


test_that("colourInput", {
    skip_on_bioc()
    expect_snapshot_output(colourpicker::colourInput("a", "a"))
})



test_that("blue  green  make_style  red funcs", {
    skip_on_bioc()
    if(crayon::has_color()){
        expect_equal(crayon::blue$bold("a"), "\033[34m\033[1ma\033[22m\033[39m")
        expect_equal(crayon::green$bold("a"),  "\033[32m\033[1ma\033[22m\033[39m")
        expect_equal(crayon::red$bold("a"), "\033[31m\033[1ma\033[22m\033[39m")
        expect_equal(crayon::make_style("orange")$bold("a"), "\033[38;5;214m\033[1ma\033[22m\033[39m")

    } else {
        expect_equal(crayon::blue$bold("a"), remove_ANSI("\033[34m\033[1ma\033[22m\033[39m"))
        expect_equal(crayon::green$bold("a"),  remove_ANSI("\033[32m\033[1ma\033[22m\033[39m"))
        expect_equal(crayon::red$bold("a"), remove_ANSI("\033[31m\033[1ma\033[22m\033[39m"))
        expect_equal(crayon::make_style("orange")$bold("a"), remove_ANSI("\033[38;5;214m\033[1ma\033[22m\033[39m"))
    }
})

test_that("dplyr funcs", {
    df <- dplyr::as_tibble(iris)
    expect_s3_class(df, "tbl")
    expect_equal(nrow(df %>% dplyr::add_row(df[1,])), 151)
    remote <- df %>%
        dplyr::filter(Sepal.Length < 5) %>%
        dplyr::select(Petal.Width)
    new_df <- collect(remote)
    expect_equal(names(new_df), "Petal.Width")
    expect_equal(nrow(new_df), 22)
    expect_equal(dplyr::pull(df, Petal.Width)[2], 0.2)
    expect_equal(df %>% dplyr::slice(5) %>% dplyr::pull(Sepal.Length), 5)
    df2 <- dplyr::tribble(
        ~colA, ~colB,
        "a",   1,
        "b",   2,
        "c",   3
    )
    expect_equal(names(df2), c('colA', 'colB'))
    expect_equal(df2[[1, 2]], 1)
})


test_that("ggplot2 funcs", {
    p <- ggplot(iris, aes(x =  Sepal.Length, y = Species)) +
        geom_bar(stat = "identity") +
        geom_point() +
        coord_flip() +
        ggtitle("test")
    expect_s3_class(p, "ggplot")
})


test_that("glue funcs", {
    a <- c("abc", "def")
    expect_equal(glue("a{a[1]}b"), "aabcb", ignore_attr = TRUE)
    expect_equal(glue_collapse(a, sep = ""), "abcdef", ignore_attr = TRUE)
})


test_that("renderMarkdown funcs", {
    expect_equal(renderMarkdown(text = "# abc"), "<h1>abc</h1>\n")
    expect_equal(renderMarkdown(text = "- abc"), "<ul>\n<li>abc</li>\n</ul>\n")
})


test_that("networkD3 funcs", {
    t_lvl <- c(1, 3, 1, 2, 2, 3)
    t_text <- c('1', '1.1.1', '2', '2.1', '2.2', '2.2.1')
    d3_tree <- step2listD3(t_lvl, t_text)
    networkD3::diagonalNetwork(d3_tree)
    expect_s3_class(renderDiagonalNetwork({}), "shiny.render.function")
})

test_that("networkD3 snap", {
    skip_on_bioc()
    expect_snapshot_output(networkD3::diagonalNetworkOutput("a"))
})


test_that("openssl funcs", {
    key <- rsa_keygen()
    msg <- serialize("1", NULL)
    encry <- encrypt_envelope(msg, key$pubkey)
    decry <- decrypt_envelope(encry$data, encry$iv, encry$session, key) %>%
        unserialize()
    expect_equal(decry, "1")
})

# skip plotly, rhandsontable, tested in main UI


test_that("rlang funcs", {
    a = 123
    enexprFun <- function(x){
        enexpr(x)
    }
    expect_equal(eval_tidy(enexprFun(a)), 123)
    expect_equal(eval_tidy(enquo(a)), 123)
    expect_equal(parse_expr("a"), rlang::expr(a))
})


test_that("rstudioapi funcs", {
    expect_true(is.logical(rstudioapi::isAvailable()))
})


test_that("shinyAce funcs", {
    expect_true(is.empty(""))
    expect_true(is.empty(NULL))
    expect_true(is.empty(NA))
    expect_true(is.empty(character()))
})


# skip shinyFiles, tested in dynamicFile
# skip shinyTree, tested in WF tab
# skip UI components from shinyWidgets, shinydashboard, shinydashboardPlus, shinyjqui
# skip shinyjs, can't test js here
# skip shinytoastr, can't test toastr.js


test_that("stringr funcs", {
    mystr <- c("this is a New new string.")
    expect_true(str_detect(mystr, "New"))
    expect_equal(str_extract(mystr, "[Nn]ew"), "New")
    expect_equal(str_remove(mystr, "New"), "this is a  new string.")
    expect_equal(str_remove_all(mystr, "[Nn]ew"), "this is a   string.")
    expect_equal(str_replace(mystr, "[Nn]ew", "a"), "this is a a new string.")
    x <- c("100a10", "100a5", "2b", "2a")
    expect_equal(str_sort(x, numeric = TRUE), c("2a", "2b", "100a5", "100a10"))
    expect_equal(
        str_split(mystr, "new", simplify = TRUE) %>% as.character(),
        c("this is a New ", " string."))
    expect_equal(str_which(x, "0"), c(1, 2))
})


test_that("styler funcs", {
    # test if this func can run
    myfile <- tempfile(fileext = ".R")
    writeLines("a=c(1,2,3)
          b <- function(x,y){
          sum(x,y)
          }", myfile)
    readLines(myfile)
    style_res <- quiet(styler::style_file(
        myfile,
        transformers = styler::tidyverse_style(
            indent_by = 4, scope = "line_breaks")
    ))
    expect_true(style_res$changed)
})


test_that("vroom func", {
    myfile <- tempfile()
    write.csv(iris, myfile, quote = FALSE, row.names = FALSE)
    mydf <- vroom::vroom(
        myfile,
        comment = "#",
        altrep = FALSE,
        delim = ",",
        col_types= vroom::cols(Sepal.Length = vroom::col_character()),
        na = character()
    )
    expect_equal(class(mydf[[1]]), "character")
    expect_equal(class(mydf[[2]]), "numeric")
    expect_equal(class(mydf$Species), "character")

    expect_warning(vroom::vroom(
        myfile,
        col_types= vroom::cols(aaa = vroom::col_character())))

})


test_that("yaml func", {
    filename <- tempfile()
    cat(
        c("a: 123", "b:", "  b1: 1", "  b2: [1, 2]", "b3: 1"),
        sep="\n",
        file=filename
    )
    yaml_load <- yaml.load_file(filename)
    expect_type(yaml_load, "list")
    expect_equal(names(yaml_load), c("a", "b", "b3"))
    expect_equal(yaml_load[["b"]][["b2"]], c(1, 2))
})
