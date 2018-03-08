# BSD_2_clause

similar_searches <- function(input, cur_input, rv) {
  cur_srch <- Search("searches", "basic", size = 10000, asdf = TRUE)$hits$hits
  srch_tms <- cur_srch$`_source.search_term`
  indices <- c()
  cur_in_spl <- unlist(
    str_split(
      gsub(cur_input(), pattern = "\"", replacement = ""),
      pattern = " "
    )
  )
  cur_qs <- cur_in_spl[!(cur_in_spl %in% tokenizers::stopwords())]
  agreps <- try(
    unlist(sapply(cur_qs, function(x) agrep(x = srch_tms, pattern = x))),
    silent = TRUE
  )

  cur_hits <- srch_tms[unique(agreps)]
  cur_hit_tab <- as.data.frame.table(
    table(cur_hits),
    stringsAsFactors = FALSE
  )
  cur_hit_tab <- filter(cur_hit_tab, cur_hits != cur_input())
  if(dim(cur_hit_tab)[1] == 0) {
    a_res <- column(4,
      actionButton(
        "no_similar_searches",
        label = "-- No similar terms --",
        value = "",
        style = "color: #757575; font-weight: 600"
      )
    )
    return(list(sim_queries = a_res,
                val_1 = NA, val_2 = NA,
                val_3 = NA, val_4 = NA,
                val_5 = NA, val_6 = NA))
  }
  cat(file=stderr(), "Have hits!\n")
  cur_hit_tab$dist <- stringdist::stringdist(
    tolower(cur_hit_tab$cur_hits),
    tolower(cur_input())
  )
  cur_hit_tab$max_dist <- sapply(
    cur_hit_tab$cur_hits,
    function(x) {
      max(nchar(x), nchar(cur_input()))
    }
  )
  cur_hit_tab$rel_dist <- cur_hit_tab$dist / cur_hit_tab$max_dist
  cat(file = stderr(), names(cur_hit_tab), "\n")
  cur_hit_arr <- arrange(cur_hit_tab, rel_dist, -Freq)
  top_6 <- head(cur_hit_arr$cur_hits)
  top_6_shrt <- substr(top_6, 1, 22)
  cat(file = stderr(), paste(top_6_shrt, collapse = " | "), "\n")
  sim_queries <- lapply(1:length(top_6_shrt), function(x) {
    column(6,
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
  val_3 <- top_6[3]
  val_4 <- top_6[4]
  val_5 <- top_6[5]
  val_6 <- top_6[6]
  return(list(sim_queries = sim_queries,
              val_1 = val_1, val_2 = val_2,
              val_3 = val_3, val_4 = val_4,
              val_5 = val_5, val_6 = val_6))
}

# run_
