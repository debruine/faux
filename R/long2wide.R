#' Long to wide format
#' 
#' Converts data from long format to wide
#' 
#' @param .data the tbl in long format
#' @param within the names of the within column(s)
#' @param between the names of between column(s) (optional)
#' @param dv the name of the DV (value) column
#' @param id the names of the column(s) for grouping observations
#' 
#' @return a tbl in wide format
#' 
#' @examples 
#' df_long <- sim_design(2, 2, long = TRUE)
#' long2wide(df_long, "A", "B", "val", "sub_id")
#' 
#' @export
#' 
long2wide <- function(.data, within = c(), between = c(), dv = "val", id = "sub_id") {
  .data %>%
    dplyr::select(tidyselect::one_of(c(id, between, within, dv))) %>%
    tidyr::unite(".tmpwithin.", tidyselect::one_of(within))  %>%
    dplyr::group_by_at(dplyr::vars(tidyselect::one_of(between))) %>%
    tidyr::spread(".tmpwithin.", !!dplyr::quo(dv)) %>%
    dplyr::ungroup()
}

#' Wide to long format
#' 
#' Converts data from wide format to long
#' 
#' @param .data the tbl in wide format
#' @param within_factors the names of the within factors
#' @param within_cols the names (or indices) of the within-subject (value) columns
#' @param sep Separator for within-columns (see tidyr::separate)
#' 
#' @return a tbl in long format
#' 
#' @examples 
#' wide2long(iris, c("Feature", "Measure"), 1:4)
#' 
#' @export
#' 
wide2long <- function(.data, within_factors = c(), within_cols = c(), sep = "[^[:alnum:]]+") {
  if (is.numeric(within_cols)) {
    within_cols <- names(.data)[within_cols]
  }
  
  .data %>%
    tidyr::gather(".tmpwithin.", "val", tidyselect::one_of(within_cols)) %>%
    tidyr::separate(".tmpwithin.", within_factors, sep = sep)
}

