# BSD_2_clause
#
# MAIN SEARCH FUNCTION; note 50-result limit at this time, very simple search
# function that needs to be beefed up with some more complex query parsing

main_search <- function(input, cur_input, cur_input_basic, min_score,
                        max_hits, cur_type) {
    hide("new_behav_alert", anim = TRUE, animType = "slide", time = 0.1)
    hide("spacer", anim = TRUE, animType = "slide", time = 0.1)
    hide("esadocs_large", anim = TRUE, animType = "fade", time = 0.1)
    hide("pad_foot", anim = TRUE, animType = "fade", time = 0.1)
    show("esadocs_small", anim = TRUE, animType = "fade", time = 0.1)
    show("top_dow", anim = TRUE, animType = "fade", time = 0.1)
    if(input$main_input == "") return(NULL)
    if(grepl(cur_input, pattern = "^id:")) {
      cur_id <- gsub(cur_input, pattern = "^id:|^id: ", replacement = "")
      the_doc <- try(docs_get("esadocs", "_all", cur_id), silent = TRUE)
      if(class(the_doc) == "try-error") return(NULL)
      res_df <- single_asdf(the_doc)
      return(res_df)
    }

    # query is fully quoted:
    if(grepl(cur_input, pattern = "^(\"|\')[[:print:]]+(\"|\')$")) {
      basic <- FALSE
      body <- list(
        min_score = min_score(),
        `_source` = list(
          excludes = "raw_txt"
        ),
        query = list(
          match_phrase = list(
            raw_txt.shingles = cur_input
          )
        ),
        size = max_hits(),
        highlight = list(
          `require_field_match` = FALSE,
          order = "score",
          fields = list(
            raw_txt.shingles = list(
              `matched_fields` = "raw_txt",
              `type` = "fvh",
              `fragment_size` = 150,
              `pre_tags` = list("<span style='font-weight:700'>"),
              `post_tags` = list("</span>")
            )
          )
        )
      )
    } else { # query is more complex
      basic <- TRUE
      body <- list(
        min_score = min_score(),
        `_source` = list(
          excludes = "raw_txt"
        ),
        query = list(
          query_string = list(
            `default_field` = "raw_txt.shingles",
            query = cur_input_basic
          )
        ),
        size = max_hits(),
        highlight = list(
          `require_field_match` = FALSE,
          order = "score",
          fields = list(
            raw_txt.shingles = list(
              `matched_fields` = "raw_txt",
              `type` = "fvh",
              `fragment_size` = 150,
              `pre_tags` = list("<span style='font-weight:700'>"),
              `post_tags` = list("</span>")
            )
          )
        )
      )
    }
    withProgress(
      message = "Searching ESAdocs...",
      value = 0.5, {
        # if(basic) {
        cur_mats <- Search(
          "esadocs",
          type = cur_type(),
          body = body)$hits$hits
        # } else {
        #   cur_mats <- Search(
        #     "esadocs",
        #     type = cur_type(),
        #     body = body)$hits$hits
        # }
    })
    if(length(cur_mats) > 0) {
      intermed_df <- result_asdf(cur_mats)
      res <- try(intermed_df$highlight <- get_highlight(cur_mats))
      if(class(res) == "try-error") {
        intermed_df$highlight <- paste0(
          "Sorry, no context highlighting available for the search <b>", cur_input,
          "</b>. Please open the linked PDF and search within to find context."
        )
      }
      intermed_df <- distinct(intermed_df, file_name, .keep_all = TRUE)
      id_str <- rand_str()
      res <- docs_create(
        index = "searches",
        type = "basic",
        id = id_str,
        body = list(
          search_term = cur_input,
          date = Sys.Date()
        )
      )
      return(intermed_df)
    } else {
      return(NA)
    }
}
