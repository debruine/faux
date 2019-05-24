#' Convert data from long to wide format
#' 
#' @param data the tbl in long format
#' @param within the names of the within column(s)
#' @param between the names of between column(s) (optional)
#' @param dv the name of the DV (value) column
#' @param id the names of the column(s) for grouping observations
#' 
#' @return a tbl in wide format
#' 
#' @examples 
#' df_long <- sim_design(2, 2, long = TRUE)
#' long2wide(df_long, "A", "B")
#' 
#' @export
#' 
long2wide <- function(data, within = c(), between = c(), dv = "y", id = "id") {
  if ("design" %in% names(attributes(data))) {
    # get parameters from design
    design <- attributes(data)$design
    
    within <- names(design$within)
    between <- names(design$between)
    dv <- names(design$dv)
    id <- names(design$id)
  }
  
  d1 <- dplyr::select(data , tidyselect::one_of(c(id, between, within, dv))) 
  if (length(within)) {
    d1 <- tidyr::unite(d1, ".tmpwithin.", tidyselect::one_of(within))
  }
  if (length(between)) {
    d1 <- dplyr::group_by_at(d1, dplyr::vars(tidyselect::one_of(between)))
  }
  if (length(within)) {
    d1 <- tidyr::spread(d1, ".tmpwithin.", !!dplyr::quo(dv))
  }
  
  dplyr::ungroup(d1)
}

#' Convert data from wide to long format
#' 
#' @param data the tbl in wide format
#' @param within_factors the names of the within factors
#' @param within_cols the names (or indices) of the within-subject (value) columns
#' @param dv the name of the dv column (defaults to "y")
#' @param id the name of the ID column(s) if they don't exist, a new column will be made
#' @param sep separator for within-columns (see tidyr::separate)
#' 
#' @return a tbl in long format
#' 
#' @examples 
#' wide2long(iris, c("Feature", "Measure"), 1:4)
#' 
#' @export
#' 
wide2long <- function(data, within_factors = c(), within_cols = c(), 
                      dv = "y", id = NULL, sep = "[^[:alnum:]]+") {
  if ("design" %in% names(attributes(data))) {
    # get parameters from design
    design <- attributes(data)$design
    
    dv <- names(design$dv)
    id <- names(design$id)
    within_factors <- names(design$within)
    within_cols <- cell_combos(design$within, dv) 
  }
  
  
  if (is.numeric(within_cols)) {
    within_cols <- names(data)[within_cols]
  }
  
  if (!is.null(id)) {
    # check if ID exists and make if not
    if (!(id %in% names(data))) {
      data[[id]] <- make_id(nrow(data))
    }
  }
  
  data %>%
    tidyr::gather(".tmpwithin.", !!dplyr::sym(dv), tidyselect::one_of(within_cols)) %>%
    tidyr::separate(".tmpwithin.", within_factors, sep = sep)
}

