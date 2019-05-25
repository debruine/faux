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

