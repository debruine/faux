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
  
  longdat <- data %>% dplyr::ungroup() %>%
    tidyr::gather(".tmpwithin.", !!dplyr::sym(dv), tidyselect::one_of(within_cols)) %>%
    tidyr::separate(".tmpwithin.", within_factors, sep = sep)
  
  if (exists("design")) {
    attributes(longdat)$design <- design
  }
  
  class(longdat) <- c("faux", "data.frame")
  
  longdat
}

