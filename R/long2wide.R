#' Long to wide format
#' 
#' Converts data from long format to wide
#' 
#' @param dat the long data frame to convert
#' @param within the names of the within column(s)
#' @param between the names of between column(s) (optional)
#' @param dv the name of the DV (value) column
#' @param id the names of the column(s) for grouping observations
#' 
#' @return the data frame in wide format
#' 
#' @export
#' 
long2wide <- function(dat, within = c(), between = c(), dv = c(), id = c()) {
  dat %>%
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
#' @param dat the wide data frame to convert
#' @param within the names of the within factors
#' @param dv the names of the DV (value) columns
#' @param sep Separator between columns (see tidyr::separate)
#' 
#' @return the data frame in long format
#' 
#' @export
#' 
wide2long <- function(dat, within_factors = c(), within_cols = c(), sep = "[^[:alnum:]]+") {
  dat %>%
    tidyr::gather(".tmpwithin.", "val", tidyselect::one_of(within_cols)) %>%
    tidyr::separate(".tmpwithin.", within_factors, sep = sep)
}