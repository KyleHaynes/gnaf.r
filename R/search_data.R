search_data <- function(d) {
    app <- shinyApp(
        ui =
            fluidPage(
                #    sidebarLayout(
                #      sidebarPanel(width = 1, # HTML("This button will open Panel 1 using <code>updateCollapse</code>."),
                #                   actionButton("p1Button", "Push Me!") #,
                #                 #   selectInput("styleSelect", "Select style for Panel 1",
                #                 #    c("default", "primary", "danger", "warning", "info", "success"))
                #      ),
                mainPanel(
                    width = 12,
                    bsCollapse(
                        id = "collapseExample", multiple = T, open = c("Setup", "Search", "Results"),
                        bsCollapsePanel("Setup", "Select / Remove variables you want to view...",
                            style = "success"
                            # , "Subset vars:"
                            , selectInput("sub_vars", NULL, names(d), selected = names(d), multiple = T, selectize = T, width = "50%")
                            , selectInput("hash_option", NULL, hasht_types, selected = hasht_types[1], multiple = F)
                        ),
                        bsCollapsePanel("Search",
                            style = "info"
                            # ---- Subset Vars ----


                            # ---- First search
                            , fluidRow(
                                column(
                                    2,
                                    selectInput("var_1", NULL, names(d), selected = NULL, multiple = T)
                                ),
                                column(
                                    1,
                                    selectInput("comp_1", NULL, gnaf.r:::comp_types, selected = gnaf.r:::comp_types[5], multiple = F)
                                ),
                                column(
                                    2, # offset = 4,
                                    textInput("val_1", NULL, value = "", width = NULL, placeholder = NULL)
                                ),
                                column(
                                    1, # offset = 4,
                                     checkboxInput("hash_select_1", label = "Hash", value = FALSE)
                                ),
                                column(
                                    3, # offset = 4,
                                    textOutput("results_1")
                                )
                            )

                            # ---- Second search
                            , fluidRow(
                                column(
                                    2,
                                    selectInput("var_2", NULL, names(d), selected = NULL, multiple = T)
                                ),
                                column(
                                    1,
                                    selectInput("comp_2", NULL, gnaf.r:::comp_types, selected = gnaf.r:::comp_types[5], multiple = F)
                                ),
                                column(
                                    2, # offset = 4,
                                    textInput("val_2", NULL, value = "", width = NULL, placeholder = NULL)
                                ),
                                column(
                                    1, # offset = 4,
                                     checkboxInput("hash_select_2", label = "Hash", value = FALSE)
                                ),
                                column(
                                    3, # offset = 4,
                                    textOutput("results_2")
                                )
                            )

                            # ---- Third search
                            , fluidRow(
                                column(
                                    2,
                                    selectInput("var_3", NULL, names(d), selected = NULL, multiple = T)
                                ),
                                column(
                                    1,
                                    selectInput("comp_3", NULL, gnaf.r:::comp_types, selected = gnaf.r:::comp_types[5], multiple = F)
                                ),
                                column(
                                    2, # offset = 4,
                                    textInput("val_3", NULL, value = "", width = NULL, placeholder = NULL)
                                ),
                                column(
                                    1, # offset = 4,
                                     checkboxInput("hash_select_3", label = "Hash", value = FALSE)
                                ),
                                column(
                                    3, # offset = 4,
                                    textOutput("results_3")
                                )
                            ),

                            # , textInput("threshold", "Threshold:", value = "1", width = NULL, placeholder = NULL)
                            selectInput("threshold", "Threshold:", as.character(1:10), selected = "1", multiple = F),
                            selectInput("group_vars", "Group variables:", names(d), multiple = T),
                            actionButton("search_action_button", label = "Search ...")

                            # End of panel 1
                        ),
                        bsCollapsePanel(
                            "Results",
                            # Panel 3
                            DT::dataTableOutput("mytable") %>% shinycssloaders::withSpinner(color = "#0dc5c1")
                        )
                    )
                )
            ),
        server =
            function(input, output, session) {
                #    output$genericPlot <- renderPlot(plot(rnorm(100)))
                #    observeEvent(input$p1Button, ({
                #      updateCollapse(session, "collapseExample", open = "Panel 1")
                #    }))
                #    observeEvent(input$styleSelect, ({
                #      updateCollapse(session, "collapseExample", style = list("Panel 1" = input$styleSelect))
                #    }))

                #    observeEvent(input$styleSelect, ({
                #      updateCollapse(session, "collapseExample", style = list("Panel 1" = input$styleSelect))
                #    }))

                # Coerce to a data.table
                if (!is.data.table(d)) {
                    d <- copy(data.table(d))
                } else {
                    d <- copy(d)
                }

                if (any(names(d) == "l")) {
                    warnings("var names `l` is reserved for use in this function, it will be overwritten")
                }

                # Coerce specific columns to characters...
                for (i in names(d)) {
                    if (class(d[[i]])[1] == "character") {
                        next
                    } else if (class(d[[i]])[1] %in% c("numeric", "integer", "factor")) {
                        d[, (i) := as.character(get(..i))]
                    } else if (class(d[[i]])[1] %in% c("Date")) {
                        d[, (i) := as.character(format(get(..i), "%Y-%m-%d"))]
                    } else if (class(d[[i]])[1] %in% c("list")) {
                        d[, (i) := as.character(sapply(get(..i), paste, collapse = ", "))]
                    }
                }


                observeEvent(input$search_action_button, {
                    l <- rep(F, nrow(d))
                    d[, l := F]

                    if (exists("tmp", where = 1)) {
                        rm(tmp, pos = 1)
                    }


                    if (input$val_1 != "") {
                        res_1 <- c()
                        for (i in 1:length(input$var_1)) {

                            # browser()
                            search_1_term <- input$val_1
                            search_1_var <- input$var_1[i]
                            search_1_orig <- input$var_1[i]

                            if(input$hash_select_1){
                                search_1_var <- paste0(input$var_1[i], "_", input$hash_option, "_hAsH")
                                tmpx <- try(d[1, ..search_1_var], silent = T)
                                if(class(tmpx) == "try-error"){
                                    browser()
                                    d[, (search_1_var) := hasht(eval(as.name(search_1_orig)), input$hash_option)]
                                }
                            search_1_term <- hasht(search_1_term, input$hash_option)
                            }

                            d[, l := l + (tmp <<- gnaf.r:::comp(eval(as.name(search_1_var)), search_1_term, input$comp_1))]
                            res_1 <- c(res_1, sum(tmp))
                        }
                        output$results_1 <- renderText({
                            as.character(paste(res_1, collapse = ", "))
                        })
                    } else {
                        if (exists("tmp", where = 1)) {
                            rm(tmp)
                        }
                        res_1 <- 0
                        output$results_1 <- renderText({
                            as.character(paste(res_1, collapse = ", "))
                        })
                    }

                    if (input$val_2 != "") {
                        res_2 <- c()
                        for (i in 1:length(input$var_2)) {
                            d[, l := l + (tmp <<- gnaf.r:::comp(eval(as.name(input$var_2[i])), input$val_2, input$comp_2))]
                            res_2 <- c(res_2, sum(tmp))
                        }
                        output$results_2 <- renderText({
                            as.character(paste(res_2, collapse = ", "))
                        })
                    } else {
                        if (exists("tmp", where = 1)) {
                            rm(tmp, pos = 1)
                        }
                        res_2 <- 0
                        output$results_2 <- renderText({
                            as.character(paste(res_2, collapse = ", "))
                        })
                    }

                    if (input$val_3 != "") {
                        res_3 <- c()
                        for (i in 1:length(input$var_3)) {
                            d[, l := l + (tmp <<- gnaf.r:::comp(eval(as.name(input$var_3[i])), input$val_3, input$comp_3))]
                            res_3 <- c(res_3, sum(tmp))
                        }
                        output$results_3 <- renderText({
                            as.character(paste(res_3, collapse = ", "))
                        })
                    } else {
                        if (exists("tmp", where = 1)) {
                            rm(tmp, pos = 1)
                        }
                        res_3 <- 0
                        output$results_3 <- renderText({
                            as.character(paste(res_3, collapse = ", "))
                        })
                    }


                    # Grouping stuff ...
                    gr <- input$group_vars
                    if (length(gr) >= 1) {
                        d[, group_flag := FALSE]
                        for (i in 1:length(gr)) {
                            gids <- d[l >= 1 & !eval(as.name(gr[i])) %in% c("", NA), get(..gr[i])]
                            d[eval(as.name(gr[i])) %in% gids, group_flag := TRUE]
                        }
                    }

                    vars <- input$sub_vars
                    # browser()
                    if (is.null(length(vars))) {
                        vars <- names(d)[1:2]
                    }
                    # browser()
                    updateSelectInput(
                        inputId = "threshold",
                        choices = 1:max(d$l),
                        selected = max(d$l, na.rm = T)
                    )

                    # browser()

                    if (length(gr) >= 1) {
                        output$mytable <- DT::renderDataTable({
                            head(d[(group_flag) | l >= as.numeric(input$threshold), c(..vars, "l")][order(-l)], 10000)
                        })
                    } else {
                        output$mytable <- DT::renderDataTable({
                            head(d[l >= as.numeric(input$threshold), c(..vars, "l")][order(-l)], 10000)
                        }) # %>% formatStyle(
                        #  2,
                        # backgroundColor = styleEqual(1, c("KYLE JOHN", "ALYSIA MIN HEE"), c('gray', 'yellow'))
                        # )
                    }
                })
            }
    )
    runApp(app)
}

## Not run:


comp_types <- c("==", "%ilike%", "%like%", "%flike%", "%plike%")
comp <- function(x, y, type = comp_types) {
    if (length(type) > 1) {
        type <- type[5]
    } 
    
    if (type == "==") {
        x == y
    } else if (type == "%ilike%") {
        x %ilike% y
    } else if (type == "%like%") {
        x %like% y
    } else if (type == "%flike%") {
        x %like% y
    } else if (type == "%plike%") {
        x %plike% y
    }
}

hasht_types <- c("soundex", "metaphone")
hasht <- function(x, type = hasht_types) {
    if (length(type) > 1) {
        type <- type[1]
    } 
    
    if (type == "soundex") {
        phonics::soundex(x)
    } else if (type == "metaphone") {
        phonics::metaphone(x)
    }
}



#' @export
like <- function(vector, pattern, ignore.case = FALSE, fixed = FALSE, perl = FALSE) {
    if (is.factor(vector)) {
        as.integer(vector) %in% grep(pattern, levels(vector),
            ignore.case = ignore.case, fixed = fixed, perl = perl
        )
    } else {
        grepl(pattern, vector, ignore.case = ignore.case, fixed = fixed, perl = perl)
    }
}

#' @export
`%plike%` <- function(vector, pattern) {
    like(vector, pattern, perl = TRUE)
}
