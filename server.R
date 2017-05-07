# BSD_2_clause

source("help_modal.R")
source("about_modal.R")
source("result_mgmt.R")
source("main_search.R")
source("similar_searches.R")

###############################################################################
# SERVER

shinyServer(function(input, output, session) {

  cur_res <- reactiveValues(cr = NULL)
  searched <- reactiveValues(srch = FALSE)
  rv <- reactiveValues(current_page = 1)

  createAlert(
    session,
    "new_behavior",
    title = "New Behavior",
    content = "We've tweaked the search behavior to improve performance. Adjust
              the <b><em>Filters</em></b> as needed for deeper searching.",
    style = "success",
    dismiss = TRUE
  )

  observeEvent(input$help, {
    help_modal()
  })

  observeEvent(input$about, {
    about_modal()
  })

  disable("no_similar_searches")

  # observer for incrementing pages
  observe({
    toggleState(id = "prevButton", condition = rv$current_page > 1)
    toggleState(id = "nextButton", condition = rv$current_page < n_pages())
    hide(selector = ".page")
    shinyjs::show("hits",
                  anim = TRUE,
                  animType = "fade",
                  time = 0.1)
  })

  navPage <- function(direction) {
    rv$current_page <- rv$current_page + direction
  }

  observeEvent(input$prevButton, navPage(-1))
  observeEvent(input$nextButton, navPage(1))
  observeEvent(input$tog_extras,
               toggle("extras",
                      anim = TRUE,
                      animType = "fade",
                      time = 0.1))

  # the number of indexed documents; could change to an option() later
  output$n_docs <- renderText({
    tmp <- try(index_stats("esadocs")$indices$esadocs$primaries$docs$count,
               silent = TRUE)
    if(class(tmp) == "try-error") {
      return("<span style='color:red; font-weight:700'>The server is down;
             please wait a few minutes and try again.
             <a href='mailto:esa@defenders.org'>Contact us</a> if the problem
             persists.")
    }
    return(paste(tmp, "documents indexed"))
  })

  # set the number of results per page
  srch_len <- reactive({
    as.numeric(input$show_n)
  })

  min_score <- reactive({
    as.numeric(input$min_score)
  })

  max_hits <- reactive({
    as.numeric(input$max_hits)
  })

  # placeholder function for cleaning input; will probably be extended
  replace_chars <- function(x) {
    x <- gsub(x, pattern = '\'|\"', replacement = "\\'", fixed = TRUE)
    return(x)
  }

  # reactive form of input$main_input
  cur_input <- reactive({
    res <- replace_chars(input$main_input)
    return(res)
  })

  # reactive form of input$main_input
  cur_input_basic <- reactive({
    res <- replace_chars(input$main_input)
    res <- gsub(res, pattern = " and ", replacement = " AND ")
    res <- gsub(res, pattern = " or ", replacement = " OR ")
    return(res)
  })

  cur_type <- reactive({
    if(input$type_filt == "all") {
      return("")
    } else {
      return(input$type_filt)
    }
  })

  # convert input$date_filt into a Date
  date_from <- reactive({
    as.Date(as.numeric(input$date_filt[1]), origin = "1970-01-01")
  })

  date_to <- reactive({
    as.Date(as.numeric(input$date_filt[2]), origin = "1970-01-01")
  })

  # cur_res <- eventReactive(input$search, {
  #   main_search(input, cur_input(), min_score, max_hits, cur_type)
  # })

  observeEvent(input$search, {
    cur_res$cr <- main_search(input, cur_input(), cur_input_basic(),
                              min_score, max_hits, cur_type)
    searched$srch <- TRUE
  })

  # eventReactive(input$search, {
  similar_search_res <- reactive({
    sim_res <- similar_searches(input, cur_input, rv)
    rv <- sim_res$rv
    sim_res
  })

  output$sim_srch <- renderUI({
    fluidRow(
      similar_search_res()$sim_queries
    )
  })

  # I honestly don't know how to handle this case programmatically...oh well.
  sim_1 <- reactive({ input$search_1 })
  observeEvent(sim_1(), {
    updateTextInput(session, "main_input", value = similar_search_res()$val_1)
    cur_res$cr <- main_search(input, similar_search_res()$val_1, min_score,
                           max_hits, cur_type)
  })

  sim_2 <- reactive({ input$search_2 })
  observeEvent(sim_2(), {
    updateTextInput(session, "main_input", value = similar_search_res()$val_2)
    cur_res$cr <- main_search(input, similar_search_res()$val_2, min_score,
                           max_hits, cur_type)
  })

  sim_3 <- reactive({ input$search_3 })
  observeEvent(sim_3(), {
    updateTextInput(session, "main_input", value = similar_search_res()$val_3)
    cur_res$cr <- main_search(input, similar_search_res()$val_3, min_score,
                           max_hits, cur_type)
  })

  sim_4 <- reactive({ input$search_4 })
  observeEvent(sim_4(), {
    updateTextInput(session, "main_input", value = similar_search_res()$val_4)
    cur_res$cr <- main_search(input, similar_search_res()$val_4, min_score,
                           max_hits, cur_type)
  })

  sim_5 <- reactive({ input$search_5 })
  observeEvent(sim_5(), {
    updateTextInput(session, "main_input", value = similar_search_res()$val_5)
    cur_res$cr <- main_search(input, similar_search_res()$val_5, min_score,
                           max_hits, cur_type)
  })

  sim_6 <- reactive({ input$search_6 })
  observeEvent(sim_6(), {
    updateTextInput(session, "main_input", value = similar_search_res()$val_6)
    cur_res$cr <- main_search(input, similar_search_res()$val_6, min_score,
                           max_hits, cur_type)
  })

  n_match <- reactive({
    a_res <- try(length(cur_res$cr[[1]]), silent = TRUE)
    if(class(a_res) != "try-error") {
      return(length(cur_res$cr[[1]]))
    } else {
      return(0)
    }
  })

  output$n_hits <- renderText({
    if(test_nulls(cur_res$cr)) {
      return("")
    } else if(n_match() > 1) {
      return(paste("About", n_match(), "matches"))
    } else {
      return(paste(n_match(), "match"))
    }
  })

  # MAIN HTML GENERATION
  hit_page <- function(i, data, pg) {
    make_href <- function(ln) {
      hypo <- "https://via.hypothes.is/"
      ccid <- "https://esadocs.cci-dev.org"
      if(!grepl(ln, pattern = "^https://esadocs.cci-dev.org")) {
        if(grepl(ln, pattern = "https://cci-dev.org")) {
          ln <- gsub(ln, pattern = "^https://cci-dev.org", replacement = ccid)
          return(paste0(hypo, ln))
        }
        if(grepl(ln, pattern = "https://defend-esc-dev.org")) {
          ln <- gsub(ln, pattern = "https://defend-esc-dev.org", replacement = ccid)
          return(paste0(hypo, ln))
        }
        if(!grepl(ln, pattern = "^https")) return(paste0(hypo, ccid, ln))
      } else {
        if(!grepl(ln, pattern = hypo)) return(paste0(hypo, ln))
      }
    }

    div(id = paste0("pg", pg),
      div(class = "search-res",
        # br(),
        fluidRow(
          column(12,
            a(href = gsub(
                ifelse(!is.na(data$pdf_path[i]),
                       make_href(data$pdf_path[i]),
                       make_href(data$link[i])),
                pattern = "?", replacement = "%3F", fixed = TRUE),
              target = "_blank",
              span(
                if(!is.null(data$title[i])) {
                  ifelse(!is.na(data$title[i]) & nchar(data$title[i]) > 10,
                         data$title[i],
                         data$file_name[i])
                } else {
                  data$file_name[i]
                },
                style = "font-size:larger;font-weight:bold"
              )
            ),
            fluidRow(
              tags$div(
                class = "justify",
                div(class = "info-div",
                    style = "text-align: center;",
                    icon("file-text-o"),
                    str_replace_all(data$type[i], "_", " ")),
                div(class = "info-div",
                    style = "text-align: center;",
                    icon("calendar"),
                    data$date[i]),
                div(class = "info-div",
                    style = "text-align: center;",
                    icon("star"),
                    paste("Score:", round(data$score[i], 3))),
                div(class = "info-div-right",
                    style = "text-align: center;",
                    if(is.na(data$link[i])) {
                      "No original online"
                    } else {
                      span(icon("external-link"),
                           a(href = data$link[i],
                           class = "info_div_a",
                           target = "_blank",
                           "Original online"))
                    }
                )
              )
            ),
            fluidRow(
              column(12,
                HTML(data$highlight[i])
              )
            ),
            fluidRow(
              tags$div(
                class = "justify",
                popify(
                  actionLink(
                    inputId = paste0("spp", i),
                    label = div(class = "tag_info",
                                "Species"),
                    style = "default"
                  ),
                  title = "Relevant species",
                  content = HTML(unlist(data$species[i])),
                  placement = "bottom",
                  trigger = "focus"
                ),
                popify(
                  actionLink(
                    inputId = paste0("state", i),
                    label = div(class = "tag_info",
                                "Geo-tags"),
                    style = "default"
                  ),
                  title = "Relevant geographic places",
                  content = HTML(unlist(data$geo[i])),
                  placement = "bottom",
                  trigger = "focus"
                ),
                popify(
                  actionLink(
                    inputId = paste0("rel_agency", i),
                    label = div(class = "tag_info",
                                "Agencies"),
                    style = "default"
                  ),
                  title = "Relevant government agencies",
                  content = data$federal_agency[i],
                  placement = "bottom",
                  trigger = "focus"
                ),
                popify(
                  actionLink(
                    inputId = paste0("tags", i),
                    label = div(class = "tag_info",
                                "Tags"),
                    style = "default"
                  ),
                  title = "Tags",
                  content = data$tags[i],
                  placement = "bottom",
                  trigger = "focus"
                ),
                tags$div(
                  class = "doc_id",
                  data$id[i]
                )
              )
            )
          )
        )
      )
    )
  }

  n_pages <- reactive({
    if(!test_nulls(res_df())) {
      # observe(print(res_df()))
      n_hits <- try(length(res_df()[,1]), silent = TRUE)
      if(class(n_hits) == "try-error") return(NULL)
      n_pages_l <- n_hits %/% srch_len()
      if(n_hits %% srch_len() != 0) {
        n_pages_l <- n_pages_l + 1
      }
      return(n_pages_l)
    } else {
      return(1)
    }
  })

  generate_hits <- function(dat) {
    n_hits <- length(dat[,1])
    page_ls <- as.list(rep(NA, n_pages()))
    pages <- 1:n_pages()
    breaks <- try(seq(1, n_hits, srch_len()), silent = TRUE)
    if(class(breaks) == "try-error") return("NOTHING")
    if(length(breaks) == 1) {
      page_ls[[1]] <- lapply(1:length(dat[, 1]), hit_page, data = dat, pg = 1)
    } else {
      for(j in pages[1:n_pages() - 1]) {
        cur_st <- breaks[pages[j]]
        cur_en <- breaks[pages[j + 1]] - 1
        cur_set <- dat[cur_st:cur_en, ]
        page_ls[[j]] <- lapply(1:length(cur_set[, 1]),
                               hit_page,
                               data = dat[cur_st:cur_en, ],
                               pg = j)
      }
      cur_st <- breaks[length(breaks)]
      cur_en <- length(dat[,1])
      page_ls[[length(pages)]] <- lapply(1:length(dat[cur_st:cur_en, 1]),
                                         hit_page,
                                         data = dat[cur_st:cur_en, ],
                                         pg = length(pages))
    }
    return(page_ls)
  }

  res_df <- reactive({
    if(!is.data.frame(cur_res$cr)) return(NULL)
    if(input$sortby == "rev_score") {
      res_dft <- dplyr::arrange(cur_res$cr, score)
    } else if(input$sortby == "date") {
      res_dft <- dplyr::arrange(cur_res$cr, date)
    } else if(input$sortby == "rev_date") {
      res_dft <- dplyr::arrange(cur_res$cr, desc(date))
    } else {
      res_dft <- cur_res$cr
      # observe({ print(length(res_dft)) })
      if(length(res_dft) == 1) {
        if(is.na(res_dft)) {
          return(NULL)
        }
      }
    }
    if(input$min_score != 0.1) {
      res_dft <- dplyr::filter(res_dft, score >= input$min_score)
    }
    if(dim(res_dft)[1] == 0) {
      return(h4("No matches greater than filter score; please adjust."))
    }
    # observe({print(dim(res_dft))})
    if(input$type_filt != "all") {
      res_dft <- dplyr::filter(res_dft, type == input$type_filt)
    }
    if(dim(res_dft)[1] == 0) {
      return(NULL)
      # return(h4("No matches for that type; please adjust type."))
    }
    return(res_dft)
  })

  output$hits <- renderUI({
    if(test_nulls(cur_res$cr) & searched$srch) {
      h4(paste("No matches for", cur_input()))
    } else if(test_nulls(cur_res$cr) & !searched$srch) {
      return("")
    } else {
      show("sim_search_h4")
      if(class(res_df()) != "data.frame" & searched$srch) {
        output$n_filt_hit <- renderText("Zero filtered hits")
        createAlert(
          session,
          anchorId = "no_filtered_hits",
          title = "No filtered hits",
          content = "No documents meet the Filter criteria. Consider making the
                    filters less stringent (for example, a higher maximum number
                    of hits, or a lower minimum score) and searching again.",
          style = "warning",
          dismiss = TRUE,
          append = FALSE
        )
        return(NULL)
        # return(res_df())
      }
      output$n_filt_hit <- reactive({
        filt_hits <- dim(res_df())[1]
        if(is.null(filt_hits)) return("")
        if(filt_hits > 1) {
          return(paste("(Filtered:", dim(res_df())[1], "hits)"))
        } else {
          return(paste("(One filtered hit)"))
        }
      })
      pages <- generate_hits(res_df())

      shinyjs::show("similar_searches_div")
      if(length(pages) > 1) {
        shinyjs::show("prevButton")
        shinyjs::show("res_txt")
        shinyjs::show("nextButton")
      } else {
        shinyjs::hide("prevButton")
        shinyjs::hide("res_txt")
        shinyjs::hide("nextButton")
      }
      if(dim(res_df())[1] > (0.85 * max_hits())) {
        createAlert(
          session,
          "more_hits",
          title = "More matches?",
          content = "You're close to the maximum number of hits filter; there
                    may be more matches of interest. Consider increasing the
                    max number of hits in <em>Filters</em> and searching again
                    for a more complete set of results.",
          style = "warning",
          dismiss = TRUE,
          append = FALSE
        )
      } else {
        hide("more_hits_div")
        # try(closeAlert(session, "more_hits"), silent = TRUE)
      }
      if(length(pages) < rv$current_page) rv$current_page <- 1
      return(pages[[rv$current_page]])
    }
  })

  output$summary_figs <- renderUI({
    if(test_nulls(cur_res$cr)) {
      shinyjs::hide("prevButton")
      shinyjs::hide("res_txt")
      shinyjs::hide("nextButton")
      shinyjs::hide("summ_head")
      shinyjs::hide("get_results")
      return(NULL)
    }
    shinyjs::show("summ_head")
    shinyjs::show("get_results")

    if(dim(cur_res$cr)[1] > 0) {
      output$doc_types <- DT::renderDataTable({
        doc_tab <- sort(table(cur_res$cr$type), decreasing = TRUE)
        types <- gsub(names(doc_tab), pattern = "_", replacement = " ")
        doc_df <- data.frame(type = types,
                             count = as.vector(doc_tab),
                             stringsAsFactors = FALSE)
        return(doc_df)
      })
    }

    output$pages_plot <- renderPlot({
      dat <- data.frame(score = as.numeric(cur_res$cr$n_pages))
      if(dim(dat)[1] == 0) return(NULL)
      nbin <- floor(dim(dat)[1] / 3)
      if(nbin < 1) nbin <- 1
      if(nbin > 30) nbin <- 30
      p <- ggplot(data = dat, aes(score)) +
             geom_histogram(bins = nbin) +
             labs(x = "# pages",
                  y = "# documents") +
             theme_hc()
      return(p)
    })

    output$date_plot <- renderPlot({
      dat <- data.frame(date = as.Date(cur_res$cr$date))
      output$n_na_date <- renderText({
        paste("# docs without a date:", sum(is.na(dat$date)) )
      })
      if(dim(dat)[1] == 0) return(NULL)
      nbin <- floor(dim(dat)[1] / 3)
      if(nbin < 1) nbin <- 1
      if(nbin > 30) nbin <- 30
      p <- ggplot(data = dat, aes(date)) +
             geom_histogram(bins = nbin) +
             labs(x = "Document date",
                  y = "# documents") +
             theme_hc()
      return(p)
    })

    output$spp_table <- DT::renderDataTable({
      spp_list <- str_split(paste(cur_res$cr$species, collapse = "<br>"), "<br>")
      spp_tab <- sort(table(spp_list), decreasing = TRUE)
      spp_df <- data.frame(taxon = names(spp_tab),
                           count = as.vector(spp_tab),
                           stringsAsFactors = FALSE)
      return(spp_df)
    })

    output$score_plot <- renderPlot({
      dat <- data.frame(score = cur_res$cr$score)
      if(dim(dat)[1] == 0) return(NULL)
      nbin <- floor(dim(dat)[1] / 3)
      if(nbin < 1) nbin <- 1
      if(nbin > 30) nbin <- 30
      p <- ggplot(data = dat, aes(score)) +
             geom_histogram(bins = nbin) +
             labs(x = "Search score",
                  y = "# documents") +
             theme_hc()
      return(p)
    })

    tabsetPanel(
      tabPanel(
        title = "Type",
        br(),
        p(style="font-size:larger", "Types of documents"),
        hr(style="border-top-color:#d9d9d9 !important; border-top-width:2px"),
        DT::dataTableOutput("doc_types", height = "250px")
      ),
      tabPanel(
        title = "Date",
        br(),
        p(style="font-size:larger", "Document dates"),
        hr(style="border-top-color:#d9d9d9 !important; border-top-width:2px"),
        plotOutput("date_plot", height = "350px"),
        div(style="text-align:right", helpText(textOutput("n_na_date")))
      ),
      tabPanel(
        title = "Species",
        br(),
        p(style="font-size:larger", "Species (taxa) in documents"),
        hr(style="border-top-color:#d9d9d9 !important; border-top-width:2px"),
        DT::dataTableOutput("spp_table", height = "250px")
      ),
      tabPanel(
        title = "Pages",
        br(),
        p(style="font-size:larger", "# pages per document"),
        hr(style="border-top-color:#d9d9d9 !important; border-top-width:2px"),
        plotOutput("pages_plot", height = "350px")
      ),
      tabPanel(
        title = "Score",
        br(),
        p(style="font-size:larger", "Document scores given search terms"),
        hr(style="border-top-color:#d9d9d9 !important; border-top-width:2px"),
        plotOutput("score_plot", height = "350px")
      ),
      type = "pills"
    )

  })

  output$get_results <- downloadHandler(filename=function() {
      "search_results.xlsx"
    },
    content=function(file) {
      cur_data <- cur_res$cr
      for_write <- make_writeable(cur_data)
      rio::export(for_write, file = file)
    }
  )

  make_writeable <- function(df) {
    to_txt <- function(x) {
      if(class(x) == "list") {
        resvec <- sapply(x, paste, collapse = "; ")
        return(resvec)
      }
      return(x)
    }

    for(i in names(df)) {
      df[[i]] <- to_txt(df[[i]])
    }
    return(df)
  }

  output$foot_spacer <- renderUI({
    HTML("<br><br><br><br><br><br><br><br><br><br><br><br><br>
         <br><br><br><br><br><br>")
  })

})
