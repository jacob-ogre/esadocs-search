# BSD_2_clause
#
# Functions to help manage elasticsearch results

single_asdf <- function(res) {
  get_var <- function(d, v) {
    if(v %in% names(d$`_source`) & length(d$`_source`[[v]] > 0)) {
      return(d$`_source`[[v]])
    }
    return(NA)
  }

  cur_dat <- data.frame(
    type = res$`_type`,
    score = 0,
    id = res$`_id`,
    title = get_var(res, "title"),
    date = get_var(res, "date"),
    file_name = get_var(res, "file_name"),
    link = get_var(res, "link"),
    pdf_path = get_var(res, "pdf_path"),
    txt_path = get_var(res, "txt_path"),
    pdf_md5 = get_var(res, "pdf_md5"),
    n_pages = get_var(res, "n_pages"),
    fr_citation_page = get_var(res, "fr_citation_page"),
    activity_code = get_var(res, "activity_code"),
    ch_status = get_var(res, "ch_status"),
    doc_type = get_var(res, "doc_type"),
    federal_agency = paste(res$`_source`$federal_agency[[1]],
                           collapse = "<br>"),
    species = paste(res$`_source`$species[[1]],
                    collapse = "<br>"),
    geo = paste(res$`_source`$geo[[1]], collapse = "<br>"),
    tags = paste(res$`_source`$tags[[1]], collapse = "<br>"),
    highlight = "<br><p>No snippet generated from a direct lookup.</p><br>",
    stringsAsFactors = FALSE
  )
  return(cur_dat)
}

result_asdf <- function(res) {
  score_ls <- vector("list", length(res))
  res_ls <- vector("list", length(res))
  id_ls <- vector("list", length(res))
  type_ls <- vector("list", length(res))
  for(i in 1:length(res)) {     # NOTE: lapply doesn't work for some reason
    score_ls[[i]] <- res[[i]]$`_score`
    id_ls[[i]] <- res[[i]]$`_id`
    type_ls[[i]] <- res[[i]]$`_type`
    # type <- get_var(res[[i]]$`_source`, "type")
    title <- get_var(res[[i]]$`_source`, "title")
    date <- get_var(res[[i]]$`_source`, "date")
    file_name <- get_var(res[[i]]$`_source`, "file_name")
    link <- get_var(res[[i]]$`_source`, "link")
    pdf_path <- get_var(res[[i]]$`_source`, "pdf_path")
    txt_path <- get_var(res[[i]]$`_source`, "txt_path")
    # raw_text <- get_var(res[[i]]$`_source`, "raw_txt")
    pdf_md5 <- get_var(res[[i]]$`_source`, "pdf_md5")
    n_pages <- get_var(res[[i]]$`_source`, "n_pages")
    fr_citation_page <-  get_var(res[[i]]$`_source`, "fr_citation_page")
    plan_act_status <-  get_var(res[[i]]$`_source`, "plan_act_status")
    plan_status <-  get_var(res[[i]]$`_source`, "plan_status")
    federal_agency <- get_var(res[[i]]$`_source`, "federal_agency")
    activity_code <-  get_var(res[[i]]$`_source`, "activity_code")
    ch_status <- get_var(res[[i]]$`_source`, "ch_status")
    doc_type <- get_var(res[[i]]$`_source`, "doc_type")
    services <- get_var(res[[i]]$`_source`, "services")
    spp_tmp <- paste(res[[i]]$`_source`$species[[1]], collapse = "<br>")
    geo <- paste(res[[i]]$`_source`$geo[[1]], collapse = "<br>")
    tags <- paste(res[[i]]$`_source`$tags[[1]], collapse = "<br>")
    cur_dat <- data.frame(title = title,
                          date = date,
                          file_name = file_name,
                          link = link,
                          pdf_path = pdf_path,
                          txt_path = txt_path,
                          # raw_txt = raw_text,
                          pdf_md5 = pdf_md5,
                          n_pages = as.numeric(n_pages),
                          fr_citation_page = fr_citation_page,
                          federal_agency = federal_agency,
                          activity_code = activity_code,
                          ch_status = ch_status,
                          doc_type = doc_type,
                          species = spp_tmp,
                          geo = geo,
                          tags = tags,
                          stringsAsFactors = FALSE)
    # names(cur_dat)[7] <- "raw_txt"
    res_ls[[i]] <- cur_dat
  }
  cur_res_df <- suppressWarnings(dplyr::bind_rows(res_ls))
  cur_res_df$score <- unlist(score_ls)
  cur_res_df$id <- unlist(id_ls)
  cur_res_df$type <- unlist(type_ls)
  return(cur_res_df)
}

get_var <- function(src, varname) {
  ifelse(is.null(src[[varname]]),
         NA, src[[varname]])
}

get_highlight <- function(res) {
  res_ls = vector("list", length(res))

  # unfortunately, lapply doesn't seem to work well over an ES result object,
  # at least not without getting way more complicated than using a for loop
  for(i in 1:length(res)) {
    if(!("highlight" %in% names(res[[i]]))) {
      res_ls[[i]] <- "Sorry, no search term highlighting available for this document."
    } else {
      hi_tmp <- lapply(res[[i]]$highlight, FUN = abbrev)
      hi_tmp <- str_replace_all(hi_tmp, "[ ]{2,}|\n", " ")
      res_ls[[i]] <- hi_tmp
    }
  }
  res_vec <- unlist(res_ls)
  return(res_vec)
}

abbrev <- function(x) {
  if(length(x) > 3) {
    paste(x[1:3], collapse = " ... ")
  } else {
    paste(x, collapse = " ... ")
  }
}

test_nulls <- function(x) {
  if(class(x) == "NULL" |
     class(x) == "NA" |
     class(x) == "logical") {
    return(TRUE)
  }
  return(FALSE)
}

rand_str <- function(len=30) {
  str <- paste(
    sample(
      c(rep(0:9,each=5),
        LETTERS,
        letters),
      len,
      replace=TRUE),
    collapse='')
  return(str)
}

