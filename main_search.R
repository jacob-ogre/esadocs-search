# BSD_2_clause
#
# MAIN SEARCH FUNCTION; note 50-result limit at this time, very simple search
# function that needs to be beefed up

main_search <- function(input, cur_input, min_score, max_hits, cur_type) {
  # eventReactive(input$search, {
    hide("new_behav_alert", anim = TRUE, animType = "slide", time = 0.1)
    # hide("more_hits_div", anim = TRUE, animType = "slide", time = 0.1)
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
    # searched <- Search(
    #   index = "presearch",
    #   type = "basic",
    #   body = list(
    #     query = list(
    #       match_phrase = list(
    #         search_term = cur_input()
    #       )
    #     )
    #   ),
    #   asdf = TRUE
    # )
    # s_res <- searched$hits$hits$`_source`
    # s_res$id <- searched$hits$hits$`_id`
    # a_match <- FALSE
    # if(!is.null(s_res)) {
    #   perf_match <- filter(s_res, search_term == cur_input())
    #   a_match <- TRUE
    # }
    # if(exists("perf_match") &&
    #    length(perf_match$search_term) > 0 &&
    #    input$use_cache == "yes") {
    #   cur_res <- readRDS(perf_match$rds_path)
    #   upd <- docs_update(
    #     index = "presearch",
    #     type = "basic",
    #     id = perf_match$id,
    #     body = list(
    #       doc = list(count = perf_match$count + 1)
    #     )
    #   )
    #   return(cur_res)
    # } else if(grepl(cur_input(), pattern = "^(\"|\')[[:print:]]+(\"|\')$")) {
    # a_match <- FALSE
    if(grepl(cur_input, pattern = "^(\"|\')[[:print:]]+(\"|\')$")) {
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
          fields = list(
            raw_txt.shingles = list(
              `type` = "fvh",
              `fragment_size` = 150,
              `pre_tags` = list("<b>"),
              `post_tags` = list("</b>")
            )
          )
        )
      )
    } else {
      body <- list(
        min_score = min_score(),
        `_source` = list(
          excludes = "raw_txt"
        ),
        query = list(
          match = list(
            raw_txt.shingles = cur_input
          )
        ),
        size = max_hits(),
        highlight = list(
          fields = list(
            raw_txt.shingles = list(
              `type` = "fvh",
              `fragment_size` = 150,
              `pre_tags` = list("<b>"),
              `post_tags` = list("</b>")
            )
          )
        )
      )
    }
    withProgress(
      message = "Searching ESAdocs...",
      value = 0.5, {
        cur_mats <- Search(
          "esadocs",
          type = cur_type(),
          body = body)$hits$hits
    })
    if(length(cur_mats) > 0) {
      intermed_df <- result_asdf(cur_mats)
      res <- try(intermed_df$highlight <- get_highlight(cur_mats))
      if(class(res) == "try-error") {
        intermed_df$highlight <- paste0(
          "Sorry, no context highlighting available for the search <b>", cur_input, "</b>.",
          " Please open the linked PDF and search within to find context."
        )
      }
      intermed_df <- distinct(intermed_df, file_name, .keep_all = TRUE)
      # if(!a_match) {
      #   id_str <- rand_str()
      #   # newf <- paste0("/home/jacobmalcom/Data/ESAdocs_presearch/", id_str, ".rds")
      #   newf <- paste0("~/Downloads/RDS_tests/", id_str, ".rds")
      #   saveRDS(intermed_df, file = newf)
      #   res <- docs_create(
      #     index = "presearch",
      #     type = "basic",
      #     id = id_str,
      #     body = list(
      #       search_term = cur_input(),
      #       count = 1,
      #       rds_path = newf,
      #       date = Sys.Date()
      #     )
      #   )
      # }
      return(intermed_df)
    } else {
      # observe({ print("483") })
      return(NA)
    }
  # })
}
