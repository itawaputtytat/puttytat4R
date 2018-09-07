#' @export
createFilterString <- function(col_name, target_values) {
  paste(col_name, "%in%",
        paste0("c(",
               paste(target_values, collapse = ","),
               ")"))
}
