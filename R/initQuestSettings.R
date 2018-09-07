#' @title Initialise settings for questionnaire
#' @export
initQuestSettings <- function(abbr,
                              db_conn_name,
                              db_src_name,
                              sett_name = "sett",
                              lang = "eng",
                              max_nchar = 99,
                              ...) {

  sett_q <- list()
  sett_q[["meta"]] <- c()
  sett_q[["meta"]][["abbr"]] <- abbr
  sett_q[["meta"]][["db_src_name"]] <- db_src_name
  sett_q[["meta"]][["lang"]] <- lang

  ## Convert meta information vector to list
  sett_q[["meta"]] <- lapply(sett_q$meta, function(x) x)

  ## Load meta info on questionnaire
  sett_q[["db_data"]] <- dbGetSrc(db_conn_name, db_src_name)

  ## Sort by item id
  sett_q[["db_data"]] <- sett_q[["db_data"]] %>% arrange_("item_id")

  ## Create new meta info data set based on selected language
  ## ... and remove language suffixes from column names
  col_finder <- names(sett_q[["db_data"]])
  col_finder <- grepl(lang, col_finder) | !grepl("ger|eng", col_finder)
  sett_q[["db_data_lang"]] <- sett_q[["db_data"]][, col_finder]
  col_names <- names(sett_q[["db_data_lang"]])
  new_names <-
    vapply(col_names,
           function(col_name) {
             sub(paste0("_", lang), "", col_name)
           },
           character(1),
           USE.NAMES = F)
  names(sett_q[["db_data_lang"]]) <- new_names

  ## Create vector of column names for items
  col_names <- sprintf(paste0(abbr, "_%02d"), 1:nrow(sett_q[["db_data"]]))
  sett_q[["items"]][["col_names"]] <- col_names

  ## Get column names for subscales
  ## ATTENTION: Language will be set so english for column naming!!!
  col_finder <- paste0("subscale_name_", "eng", "_abbr")
  col_names <- paste_(abbr, tolower(sett_q[["db_data"]][, col_finder]))
  sett_q[["subscales"]][["col_names"]] <- col_names

  ## Get unique column names for subscales
  col_names <- paste_(abbr, tolower(sett_q[["db_data"]][, col_finder]))
  col_names <- sort(unique(col_names))
  sett_q[["subscales"]][["col_names_unique"]] <- col_names

  ## Get overview on subscale
  ## Data frame will be sorted in order to ensure correct level assignment
  ## ... when using other languages
  col_finder <- grepl("subscale_name", names(sett_q$db_data))
  sett_q[["subscales"]][["overview"]] <-
    data.frame(col_names = sett_q[["subscales"]][["col_names"]],
               sett_q$db_data[, col_finder]) %>%
    distinct() %>%
    arrange(col_names)

  ## Get subscale abbrebiations for active language
  col_finder <- grepl(lang, names(sett_q[["subscales"]][["overview"]]))
  temp <- sett_q[["subscales"]][["overview"]][, col_finder]
  sett_q[["subscales"]][["abbr_lang"]] <- temp

  ## Get subscale names for active language
  col_finder <-
    grepl(lang, names(sett_q[["subscales"]][["overview"]])) &
    !grepl("abbr", names( sett_q[["subscales"]][["overview"]]))
  temp <- sett_q[["subscales"]][["overview"]][, col_finder]
  sett_q[["subscales"]][["names_lang"]] <- temp

  ## Get general subscale abbreviations and subscale names for active language
  col_finder1 <-
    !grepl(lang, names(sett_q[["subscales"]][["overview"]])) &
    grepl("abbr", names( sett_q[["subscales"]][["overview"]]))
  col_finder1 <- names(sett_q[["subscales"]][["overview"]])[col_finder1]
  col_finder2 <-
    grepl(lang, names(sett_q[["subscales"]][["overview"]])) &
    !grepl("abbr", names( sett_q[["subscales"]][["overview"]]))
  col_finder2 <- names(sett_q[["subscales"]][["overview"]])[col_finder2]
  col_finder <- c(col_finder1, col_finder2)
  temp <- sett_q[["subscales"]][["overview"]][, col_finder]
  sett_q[["subscales"]][["abbr_eng_and_names_lang"]] <- temp
  col_names <- names(sett_q[["subscales"]][["abbr_eng_and_names_lang"]])
  col_names[2] <- "subscale_name"
  names(sett_q[["subscales"]][["abbr_eng_and_names_lang"]]) <- col_names


  ## Create list of subscales containing column names of items
  dat_temp <-
    data.frame(
      col_name = sett_q[["items"]][["col_names"]],
      subscale_abbr = sett_q[["subscales"]][["col_names"]],
      stringsAsFactors = F
    ) %>%
    split(.[,"subscale_abbr"]) %>%
    lapply(., "[[", 1)
  sett_q[["subscales"]][["items"]] <- dat_temp

  ## Get scale values
  sett_q[["scale"]][["values"]] <-
    as.numeric(unlist(strsplit(sett_q[["db_data"]][1, "scale_values"], ";")))
  sett_q[["scale"]][["values_min"]] <- min(sett_q[["scale"]][["values"]])
  sett_q[["scale"]][["values_max"]] <- max(sett_q[["scale"]][["values"]])
  sett_q[["scale"]][["values_range"]] <- range(sett_q[["scale"]][["values"]])

  ## Create vector of columns to be is_reversed
  if ("is_reversed" %in% colnames(sett_q[["db_data"]])) {
    col_finder <- sett_q[["db_data"]][, "is_reversed"]
    sett_q[["items"]][["col_names_to_reverse"]] <-
      sett_q[["items"]][["col_names"]][col_finder]
  }

  ## Create data frame for item texts
  sett_q[["items"]][["texts"]] <-
    data.frame(
      col_name = sett_q[["items"]][["col_names"]],
      item_text = sett_q[["db_data_lang"]][, "item_text"],
      stringsAsFactors = F,
      row.names = NULL
    )

  ## Create data frame for item texts with line breaks
  ## Convert item text to factor to keep level order
  text <-
    breakStringToLines(
      sett_q[["db_data_lang"]][, "item_text"],
      max_nchar, ...
    )

  text <- factor(text, levels = text)

  sett_q[["items"]][["texts_with_line_break"]] <-
    data.frame(
      col_name = sett_q[["items"]][["col_names"]],
      item_text = text,
      stringsAsFactors = F,
      row.names = NULL
    )

  ## Create final data and append data, if object for settings already exists
  if (is.null(sett_name)) {
    sett_q_final <- list()
    sett_q_final[[sett_q[["meta"]][["abbr"]]]] <- sett_q
    return (sett_q_final)
  } else {

    ## Check if settings objects is available
    if (exists(sett_name, env = .GlobalEnv)) {
      sett_q_final <- get(sett_name, .GlobalEnv)
    } else {
      sett_q_final <- list()
    }

    ## Check if quest element is available
    if (length(sett_q_final[["quest"]]) != 0) {


    } else {
      sett_q_final[["quest"]] <- list()
      sett_q_final[["quest"]][[abbr]] <- list()
    }

    ## Check if specific questionnaire settings are available
    if (length(sett_q_final[["quest"]][[abbr]]) != 0) {
      outputString(paste("* Object", sett_name, "already exists"))
      outputString(paste("** Results for questionnaire will be overwritten"))
    } else {
      sett[["quest"]][[abbr]] <- list()
    }

    sett_q_final[["quest"]][[abbr]] <- sett_q
    assign(sett_name, sett_q_final, env = .GlobalEnv)
  }
}
