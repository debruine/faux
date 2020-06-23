#' Get parameters from a data table
#'
#' \code{get_params} Generates a table of the correlations and means of numeric columns in a data frame
#'
#' @param data the existing tbl
#' @param between a vector of column names for between-subject factors
#' @param within a vector of column names for within-subject factors (if data is long)
#' @param dv the column name of the dv (if data is long)
#' @param id the column name(s) of the subject ID (if data is long)
#' @param digits how many digits to round to (default = 2)
#' 
#' @return a tbl of correlations, means and sds
#' @examples
#' get_params(iris, "Species")
#' @export
#' 

get_params <- function(data, between = c(), within = c(), 
                       dv = "y", id = "id", digits = 2) {
  
  if (length(within) && length(dv) && length(id)) {
    # convert long to wide
    data <- long2wide(data, within, between, dv, id) %>%
      dplyr::select(-tidyselect::one_of(id))
  }
  
  grpdat <- select_num_grp(data, between)
  grpvars <- dplyr::group_vars(grpdat)
  numvars <- names(grpdat)[!names(grpdat) %in% grpvars]
  
  descriptives <- dplyr::bind_rows(
    dplyr::summarise_all(grpdat, ~mean(.)) %>% dplyr::mutate(stat = "mean"),
    dplyr::summarise_all(grpdat, ~stats::sd(.)) %>% dplyr::mutate(stat = "sd"),
    dplyr::summarise_all(grpdat, ~dplyr::n()) %>% dplyr::mutate(stat = "n")
  ) %>%
    tidyr::gather("var", "val", dplyr::one_of(numvars)) %>%
    dplyr::mutate("val" = round(.data$val, digits)) %>%
    tidyr::spread(.data$stat, .data$val)
  
  stats <- grpdat %>%
    tidyr::nest("multisim_data" = tidyselect::one_of(numvars)) %>%
    dplyr::mutate("multisim_cor" = purrr::map(.data$multisim_data, function(d) {
      cor(d) %>% round(digits) %>% tibble::as_tibble(rownames = "var")
    })) %>%
    dplyr::select(-.data$multisim_data) %>%
    tidyr::unnest(cols = "multisim_cor") %>%
    dplyr::left_join(descriptives, by = c(between, "var")) %>%
    dplyr::select(tidyselect::one_of(c(between, "n", "var", numvars, "mean", "sd"))) %>%
    dplyr::ungroup()
    
  stats
}

#' @rdname get_params
#' @export
check_sim_stats <- get_params
