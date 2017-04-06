# BSD_2_clause

similar_searches <- function(input, cur_input, rv) {
  cur_srch <- Search("searches", "basic", size = 10000, asdf = TRUE)$hits$hits$`_source`
  srch_tms <- cur_srch$search_term
  indices <- c()
  cur_in_spl <- unlist(
    str_split(
      gsub(cur_input(), pattern = "\"", replacement = ""),
      pattern = " "
    )
  )
  cur_qs <- cur_in_spl[!(cur_in_spl %in% tokenizers::stopwords())]
  agreps <- unlist(sapply(cur_qs, function(x) agrep(x = srch_tms, pattern = x)))
  cur_hits <- cur_srch[unique(agreps), ]
  cur_hit_tab <- as.data.frame.table(
    table(cur_hits$search_term),
    stringsAsFactors = FALSE
  )
  cur_hit_tab <- filter(cur_hit_tab, cur_hit_tab$Var1 != cur_input())
  cur_hit_arr <- arrange(cur_hit_tab, -Freq)
  top_6 <- head(cur_hit_arr$Var1)
  sim_queries <- lapply(1:length(top_6), function(x) {
    column(4,
      actionButton(
        paste0("search_", x),
        label = top_6[x],
        value = top_6[x],
        style = "color: #1A237E; font-weight: 600"
      )
    )
  })
  val_1 <- top_6[1]
  val_2 <- top_6[2]
  return(list(sim_queries = sim_queries, val_1 = val_1, val_2 = val_2))
}

# run_
