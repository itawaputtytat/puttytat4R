#' initQuestSettings <- function(abbr,
#'                               db_conn_name,
#'                               db_src_name,
#'                               sett_name = NULL,
#'                               lang = "eng",
#'                               max_nchar = 99,
#'                               ...) {
#'
#'   sett_q <- list()
#'   sett_q[["abbr"]] <- abbr
#'   sett_q[["db_src_name"]] <- db_src_name
#'   sett_q[["lang"]] <- lang
#'
#'   ## Load meta info on questionnaire
#'   sett_q[["db_data"]] <- dbGetSrc(db_conn_name, db_src_name)
#'   sett_q[["db_data"]] <-
#'     sett_q[["db_data"]] %>%
#'     arrange_("item_id")
#'
#'   ## Create new meta info data set based on selected language
#'   ## ... and remove language suffixes from column names
#'   col_finder <- names(sett_q[["db_data"]])
#'   col_finder <- grepl(lang, col_finder) | !grepl("ger|eng", col_finder)
#'   sett_q[["db_data_lang"]] <- sett_q[["db_data"]][, col_finder]
#'   col_names <- names(sett_q[["db_data_lang"]])
#'   names(sett_q[["db_data_lang"]]) <-
#'     vapply(col_names,
#'            function(col_name) sub(paste0("_", lang), "", col_name),
#'            character(1),
#'            USE.NAMES = F)
#'
#'   ## Create vector of column names for items
#'   sett_q[["col_names"]] <-
#'     sprintf(paste0(abbr, "_%02d"), 1:nrow(sett_q[["db_data"]]))
#'
#'   ## Obtain names for subscales
#'   col_finder <- paste0("subscale_name_", lang, "_abbr")
#'   sett_q[["subscale_names"]] <-
#'     paste_(abbr, tolower(sett_q[["db_data"]][, col_finder]))
#'   sett_q[["subscale_names_unique"]] <-
#'     unique(paste_(abbr, tolower(sett_q[["db_data"]][, col_finder])))
#'
#'   ## Create list of subscales containing column names of items
#'   sett_q[["subscale_items"]] <-
#'     data.frame(col_name = sett_q[["col_names"]],
#'                subscale_abbr = sett_q[["subscale_names"]],
#'                stringsAsFactors = F) %>%
#'     split(.[,"subscale_abbr"]) %>%
#'     lapply(., "[[", 1)
#'
#'   ## Get scale values
#'   sett_q[["scale_values"]] <-
#'     as.numeric(unlist(strsplit(sett_q[["db_data"]][1, "scale_values"], ";")))
#'   sett_q[["scale_values_min"]] <- min(sett_q[["scale_values"]])
#'   sett_q[["scale_values_max"]] <- max(sett_q[["scale_values"]])
#'
#'   ## Create vector of columns to be is_reversed
#'   if ("is_reversed" %in% colnames(sett_q[["db_data"]])) {
#'     sett_q[["col_names_to_reverse"]] <-
#'       sett_q[["col_names"]][sett_q[["db_data"]][, "is_reversed"]]
#'   }
#'
#'   ## Create data frame for item texts
#'   sett_q[["item_texts"]] <-
#'     data.frame(
#'       col_name = sett_q[["col_names"]],
#'       item_text = sett_q[["db_data_lang"]][, "item_text"],
#'       stringsAsFactors = F,
#'       row.names = NULL
#'     )
#'
#'   ## Create data frame for item texts with line breaks
#'   ## Convert item text to factor to keep level order
#'   sett_q[["item_texts_with_line_breaks"]] <-
#'     breakStringToLines(sett_q[["db_data_lang"]][, "item_text"],
#'                        max_nchar, ...)
#'
#'   sett_q[["item_texts_with_line_breaks"]] <-
#'     data.frame(
#'       col_name = sett_q[["col_names"]],
#'       item_text = sett_q[["item_texts_with_line_breaks"]],
#'       stringsAsFactors = F,
#'       row.names = NULL
#'     )
#'
#'   sett_q[["item_texts_with_line_breaks"]][, "item_text"] <-
#'     factor(
#'       sett_q[["item_texts_with_line_breaks"]][, "item_text"],
#'       levels = sett_q[["item_texts_with_line_breaks"]][, "item_text"])
#'
#'   ## Create final data and append data, if object for settings already exists
#'   if (is.null(sett_name)) {
#'     sett_q_final <- list()
#'     sett_q_final[[sett_q[["abbr"]]]] <- sett_q
#'     return (sett_q_final)
#'   } else {
#'
#'     if (exists(sett_name, env = .GlobalEnv)) {
#'       outputString(paste("* Object", sett_name, "already exists"))
#'       outputString(paste("** Results will be overwritten or appended"))
#'       sett_q_final <- get(sett_name, env = .GlobalEnv)
#'     } else {
#'       sett_q_final <- list()
#'     }
#'     sett_q_final[[abbr]] <- sett_q
#'
#'     assign(sett_name, sett_q_final, env = .GlobalEnv)
#'
#'   }
#'
#' }
