# BSD_2_clause

#############################################################################
# Define the header and sidebar (disabled)
header <- dashboardHeader(disable = TRUE)
sidebar <- dashboardSidebar(disable = TRUE)

#############################################################################
# Define the page(s) with dashboardBody
body <- dashboardBody(fluidPage(
  div(class = "navbar navbar-inverse navbar-fixed-top",
    div(class="container-fluid",
      div(
        class = "navbar-header",
        tags$button(
          type = "button",
          class = "navbar-toggle collapsed",
          `data-toggle` = "collapse",
          `data-target` = "#navbar-collapse",
          `aria-expanded` = "false",
          span(class = "sr-only", 'Toggle navigation'),
          span(class = "icon-bar"),
          span(class = "icon-bar"),
          span(class = "icon-bar")
        ),
        tags$a(
          class = "navbar-brand",
          style = "font-weight:900; font-size:large",
          href = "https://cci-dev.org",
          tabindex = -1,
          "CCI-dev"
        )
      ),
      div(
        class = "collapse navbar-collapse",
        id = "navbar-collapse",
        tags$ul(
          class = "nav navbar-nav",
          tags$li(
            tags$a(
              href = "http://www.defenders.org/innovation",
              target = "_blank",
              style = "font-weight:500; font-size:large;",
              tabindex = -1,
              "Defenders"
            )
          )
        ),
        tags$ul(
          class = "nav navbar-nav navbar-right",
          style = "padding-top:10px; color:#9D9D9D",
          tags$li(
            span(
              tags$a(
                style = "color:#9D9D9D",
                href = "https://github.com/jacob-ogre/esadocs-search",
                target = "_blank",
                tabindex = -1,
                shiny::icon("github fa-2x")
              )
            )
          ),
          tags$li(
            span(
              actionLink(
                inputId = "about",
                label = "About",
                tabindex = -1,
                style = "color:#9D9D9D; font-size:large; padding-left:10px",
                icon = NULL
              )
            )
          )
        )
      )
    )
  ),
  div(class = "outer",
    shinyjs::useShinyjs(),
    # tags$style(appCSS),
    tags$head(
      HTML("<link href='https://fonts.googleapis.com/css?family=Open+Sans:300,400,700,900'
           rel='stylesheet' type='text/css'>"),
      HTML('<link rel="icon" type="image/png" href="favicon-32x32.png" sizes="32x32" />'),
      tags$script(src = "google_analytics.js"),
      tags$style(HTML(readLines("www/custom_styles.css"))),
      tags$script(src = "enter_button.js")
    ),

    br(),
    fluidRow(
      div(
        id = "spacer",
        br(), br(), br(), br(), br(), br(),
        div(
          id = "esadocs_large",
          tags$a(
            href="https://esadocs.cci-dev.org",
            tabindex = "-1",
            img(src = "ESAdocs_search.svg",
                height = "140px")
          )
        )
      )
    ),
    fluidRow(
      column(1,
        hidden(
          div(id = "esadocs_small",
            br(), br(),
            tags$a(href="https://esadocs.cci-dev.org",
              img(src = "ESAdocs_search.svg",
                  height = "80px")
            )
          )
        )
      ),
      column(10,
        br(), br(),
        fluidRow(
          column(2),
          column(8,
            fluidRow(
              div(
                class = "input-group",
                style = "padding:20px",
                textInput(
                  inputId = "main_input",
                  label = NULL,
                  placeholder = "See Help for search tips",
                  width = "100%"),
                span(
                  class = "input-group-btn",
                  actionButton(
                    inputId = "search",
                    label = NULL,
                    icon = icon("search"),
                    style = "primary",
                    style="font-size: 150%;
                           color: white;
                           background-color: #0E3670;
                           border-color: #2e6da4"
                  )
                )
              )
            # ),
            # fluidRow(
            #   br(),
            #   div(id = "new_behav_alert",
            #     bsAlert("new_behavior")
            #   )
            )
          ),
          column(2,
            fluidRow(
              actionButton(
                inputId = "help",
                label = "Help",
                icon = icon("question")
              )
            ),
            fluidRow(
              actionButton(
                inputId = "tog_extras",
                label = "Filters",
                icon = icon("filter"),
                size = "small",
                type = "toggle",
                value = FALSE
              )
            )
          )
        ),
        # fluidRow(textOutput("testing_msg")),
        fluidRow(
          hidden(
            div(id = "extras",
              column(2,
                div(class = "slim",
                  selectInput(
                    "show_n",
                    label = NULL,
                    choices = list(
                      "5 hits/page" = 5,
                      "10 hits/page" = 10,
                      "20 hits/page" = 20,
                      "50 hits/page" = 50,
                      "100 hits/page" = 100),
                    width = "95%",
                    selected = 10,
                    multiple = FALSE)
                )
              ),
              column(2,
                selectInput(
                  inputId = "sortby",
                  label = NULL,
                  choices = list(
                    "Sort Score" = "score",
                    "Rev. Score" = "rev_score",
                    "Date" = "date",
                    "Rev. Date" = "rev_date"
                  ),
                  selected = "score",
                  width = "95%",
                  multiple = FALSE
                )
              ),
              column(2,
                dateRangeInput(
                  "date_filt",
                  label = NULL,
                  start = as.Date("1967-01-01"),
                  end = Sys.Date(),
                  format = "d-M-yy",
                  width = "95%"
                )
              ),
              column(2,
                selectInput(
                  "type_filt",
                  label = NULL,
                  choices = list(
                    "All document types" = "all",
                    "Candidates" = "candidate",
                    "Conserv. Agreements" = "conserv_agmt",
                    "Consultation" = "consultation",
                    "Critical Habitat" = "critical_habitat",
                    "Federal Register" = "federal_register",
                    "Miscellaneous" = "misc",
                    "Policies" = "policy",
                    "Recovery Plan" = "recovery_plan",
                    "5-year review" = "five_year_review",
                    "7(a)(1)" = "section_7a1"
                  ),
                  width = "95%"
                )
              ),
              column(2,
                selectInput(
                  "min_score",
                  label = NULL,
                  choices = list(
                    "Min score = 0.1" = 0.1,
                    "Min score = 0.5" = 0.5,
                    "Min score = 1" = 1,
                    "Min score = 5" = 5,
                    "Min score = 10" = 10,
                    "No filter (0)" = 0
                  ),
                  width = "95%"
                )
              ),
              column(2,
                selectInput(
                  "max_hits",
                  label = NULL,
                  choices = list(
                    "Max hits = 50" = 50,
                    "Max hits = 100" = 100,
                    "Max hits = 500" = 500,
                    "Max hits = 1000" = 1000,
                    "Max hits = 5000" = 5000 #,
                    # "Max hits = 10000" = 10000
                  ),
                  width = "95%",
                  selected = 50
                )
              )
            )
          )
        ),

        fluidRow(
          column(7,
            fluidRow(
              tags$div(
                class = "justify",
                textOutput("n_hits"),
                textOutput("n_filt_hit"),
                hidden(
                  tipify(
                    downloadButton("get_results", "Download"),
                    title = "Download an Excel file of these results.",
                    placement = "right"
                  )
                )
              )
            ),
            hidden(uiOutput("hits")),
            br(),
            bsAlert("no_filtered_hits")
          ),
          column(1),
          column(4,
            hidden(
              fluidRow(helpText(htmlOutput("n_docs")))
            ),
            fluidRow(
              uiOutput("summary_figs", height = "300px")
            )
          )
        ),
        fluidRow(
          column(7,
            fluidRow(
              br(),
              hidden(
                div(
                  id = "similar_searches_div",
                  style = "margin: 0 15px",
                  column(12,
                    id = "similar_search_box",
                    h4(id = "sim_search_h4", "Similar searches"),
                    uiOutput("sim_srch")
                  )
                )
              )
            ),
            br(),
            div(
              id = "nextprev",
              style = "width:50%; margin:0 auto;",
              div(style = "display: inline-block",
                hidden(
                  actionButton(
                    "prevButton",
                    label = span(
                      style = "font-size: larger",
                      "< Previous"
                    ),
                    style = "default",
                    size = "small"
                  )
                )
              ),
              hidden(
                span(
                  id = "res_txt",
                  "Results",
                  style = "font-weight:bold; font-size: large;
                          vertical-align: middle; display:inline-block"
                )
              ),
              div(style = "display: inline-block",
                hidden(actionButton("nextButton",
                         label = span(
                           style = "font-size:larger",
                           "Next >"
                         ),
                         style = "default",
                         size = "small"))
              )
            ),
            fluidRow(
              div(
                id = "more_hits_div",
                style = "padding: 0 15px;",
                br(),
                bsAlert("more_hits")
              )
            )
          ),
          column(4)
        ),
        fluidRow(
          br(), br()
        )
      ),
      column(1)
    ),
    fluidRow(
      div(
        id = "pad_foot",
        br(), br(), br(), br()
      ),
      hidden(div(
        br(), br(), br()
      ))
    ),
    fluidRow(
      column(5),
      column(1,
        HTML('
          <a href="http://defenders.org">
          <img style="vertical-align:middle" src="DOW_logo_small.png" height="60"></a>
        ')
      ),
      column(1,
        HTML('
          <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">
          <img alt="Creative Commons License" style="border-width:0;padding-top:15px" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a>
        ')
      ),
      column(5)
    ),
    fluidRow(
      column(1),
      column(10,
        div(
          style = "text-align:center",
          HTML('<footer>
            <br />
            <p>This <span xmlns:dct="http://purl.org/dc/terms/" href="http://purl.org/dc/dcmitype/InteractiveResource" rel="dct:type">work</span>
            by <a xmlns:cc="http://creativecommons.org/ns" href="http://defenders.org" property="cc:attributionName" rel="cc:attributionURL">Defenders of Wildlife</a>
            is licensed under a <a rel="license" href="https://creativecommons.org/licenses/by-nc-sa/4.0/legalcode">Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License</a>.</p>
            <br />
          </footer>'),
          br()
        )
      ),
      column(1)
    )
  )
))

# dashboardPage(header, sidebar, body, skin="blue")
body
