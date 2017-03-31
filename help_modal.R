help_modal <- function() {
  showModal(
    modalDialog(
      title = "Getting started",
      div(style="font-size:larger",
        div(style="background-color:#f2f2f2; padding:5px",
          p("ESAdocs Search makes it easy to find documents related to the
            U.S. Endangered Species Act (ESA). To get started:"),
          tags$ol(
            tags$li("Enter your search term"),
            tags$li("Press the magnifier to search")
          ),
          HTML("<img src='new_img.png' width='90%' style='padding-left:5px'>"),
          p("Notice that the search can be refined with the Filter (red arrow)"),
          hr(),
          p(strong("Note 1:"), "Searches may take several seconds depending on
            the specifics of the terms you are looking for or the max number of
            results you allow."),
          p(strong("Note 2:"), "Use quotes, \", around a phrase to search for
            that exact phrase. These phrase searches are also considerably
            faster than a 'normal' full-text search."),
          hr()
        ),
        div(style="padding:5px",
          h3("Filters"),
          p("Filters can be applied to broaden or narrow the search results.
            Currently, filters include:"),
          tags$ol(
            tags$li("How many results to show per page"),
            tags$li("A date range to restrict results"),
            tags$li("The type of documents to search"),
            tags$li("The minimum search score to include"),
            tags$li("The maximum number of hits to include")
          ),
          HTML("<img src='new_filter_img.png' width='100%'>"),
          p("The more lenient the filters, the longer a search will take."),
          hr()
        ),
        div(style="background-color:#f2f2f2; padding:5px",
          h3("Results"),
          p("The results are pretty simple:"),
          tags$ol(
            tags$li("Each document includes a link to the PDF, a text snippet,
                  and additional metadata"),
            tags$li("The 'green line' includes the document type, date, score
                    given the search, and a link to the original version (if
                    available)"),
            tags$li("The 'blue line' includes extracted pieces of data,
                    including species, places, gov't agencies, tags, and (in gray)
                    the internal document ID."),
            tags$li("Several summaries across all hits are calculated in the
                    supplemental results sidebar. The exact contents will evolve
                    as the app is used.")
          ),
          HTML("<img src='searched_img.png' width='100%'>"),
          p("We note that the details of search results may change."),
          hr()
        ),
        div(style="padding:5px",
          h3("Advanced search tips"),
          tags$ul(
            tags$li("If you want one document, use 'id: <document ID>' for phrase"),
            tags$li("More to come...")
          )
        )
      ),
      size = "l",
      easyClose = TRUE
    )
  )
}
