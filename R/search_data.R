#' @name search_data

#' @title Shiny convenience GUI for searching data

#' @description Shiny convenience GUI for searching data


#' @param d data.frame type object

#' @author Kyle Haynes, \email{kyle@@kylehaynes.com.au}.


#' @import data.table
#' @import shiny
#' @import shinyBS
#' @import DT
#' @import varhandle
#' @import shinycssloaders
#' @export

# For reference, following was the feb link:
# https://data.gov.au/data/dataset/19432f89-dc3a-4ef3-b943-5326ef1dbecc/resource/4b084096-65e4-4c8e-abbe-5e54ff85f42f/download/feb20_gnaf_pipeseparatedvalue.zip

# library(shiny)
# library(shinyBS)
# library(DT)
# library(data.table)
# library(generator)
# n = 1000
# d <- data.table(name = toupper(r_full_names(n))
#              , alias_name_1 = toupper(r_full_names(n))
#              , alias_name_2 = toupper(r_full_names(n))
#              , alias_name_3 = toupper(r_full_names(n))
#              , phone_1 = r_phone_numbers(n)
#              , phone_2 = r_phone_numbers(n)
#              , phone_3 = r_phone_numbers(n)
#              , group_1 = sample(c(1:30, rep(NA, 200)), n, replace = T)
#              , group_2 = sample(c(1:30, rep(NA, 200)), n, replace = T)
#              , dob = r_date_of_births(n, start = as.Date("1900-01-01"), end = Sys.Date()))

# d <- iris
# d <- data.frame(lapply(d, as.character), stringsAsFactors=FALSE)
# setDT(d)
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
                                    3, # offset = 4,
                                    textOutput("results_3")
                                )
                            ),
                            
                            # , textInput("threshold", "Threshold:", value = "1", width = NULL, placeholder = NULL)
                            selectInput("threshold", "Threshold:", as.character(1:10), selected = "1", multiple = F),
                            selectInput("group_vars", "Group variables:", names(d), multiple = T),
                            actionButton("action", label = "Search ...")

                            # End of panel 1
                        ),
                        bsCollapsePanel(
                            "Results",
                            # Panel 3
                            DT::dataTableOutput("mytable") %>% withSpinner(color = "#0dc5c1")
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
                if(!is.data.table(d)){
                    d <- copy(data.table(d))
                } else {
                    d <- copy(d)
                }

                if(any(names(d) == "l")){
                    warnings("var names `l` is reserved for use in this function, it will be overwritten")
                }

                # Coerce specific columns to characters...
                for(i in names(d)){

                    if(class(d[[i]])[1] == "character"){
                        next
                    } else if(class(d[[i]])[1] %in% c("numeric", "integer", "factor")){
                        d[, (i) := as.character(get(..i))]
                    } else if(class(d[[i]])[1] %in% c("Date")){

                        d[, (i) := as.character(format(get(..i), "%Y-%m-%d"))]
                    } else if(class(d[[i]])[1] %in% c("list")){

                        d[, (i) := as.character(sapply(get(..i), paste, collapse = ", "))]
                    }
                }


                observeEvent(input$action, {

                    l <- rep(F, nrow(d))
                    d[, l := F]

                    if (input$val_1 != "") {
                        res_1 <- c()
                        for (i in 1:length(input$var_1)) {
                            d[, l := l + (tmp <<- gnaf.r:::comp(eval(as.name(input$var_1[i])), input$val_1, input$comp_1))]
                            res_1 <- c(res_1, sum(tmp))
                        }
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
    } else if (type == "==") {
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
