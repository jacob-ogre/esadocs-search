about_modal <- function() {
  showModal(
    modalDialog(
      title = "About ESAdocs Search",
      tags$p(
        style="font-size:larger",
        "ESAdocs Search is the most comprehensive tool for searching the text of
        documents on the U.S. Endangered Species Act.  It now covers over
        14,000 documents, including all ESA recovery plans, five-year status
        reviews, and Federal Register notices from the websites of the U.S. Fish
        and Wildlife Service and the National Marine Fisheries Service.  It also
        includes over 4,000 biological opinions, hundreds of conservation plans,
        and many other ESA-related documents.  We are constantly adding documents
        to this resource.  If you are interested in contributing documents to
        the database, please ",
        tags$a(
          style = "color:blue;",
          href = "mailto:esa@defenders.org",
          "email us."
        )
      ),
      tags$p(
        style = "font-size:larger",
        "ESAdocs Search is a project of the ",
        tags$a(
          style = "color:blue",
          href = "http://www.defenders.org/innovation",
          "Center for Conservation Innovation"
        ),
        "at Defenders of Wildlife."
      ),
      size = "l",
      easyClose = TRUE
    )
  )
}
