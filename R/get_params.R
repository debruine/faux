#' Check table parameters
#'
#' \code{get_params} Generates a table of the correlations and means of numeric columns in a data frame
#'
#' @param .data the existing tbl
#' @param between a vector of column names for between-subject factors
#' @param within a vector of column names for within-subject factors (if data is long)
#' @param dv the column name of the dv (if data is long)
#' @param id the column name(s) of the subject ID (if data is long)
#' @param digits how many digits to round to (default = 2)
#' @param usekable logical. If TRUE, output with knitr::kable
#' 
#' @return a tbl or kable
#' @examples
#' get_stats(iris, "Species")
#' @export
#' 

get_params <- function(.data, between = c(), within = c(), dv = "val", id = "sub_id",
                            digits = 2, usekable = FALSE) {
  
  if (length(within) && length(dv) && length(id)) {
    # convert long to wide
    .data <- long2wide(.data, within, between, dv, id) %>%
      dplyr::select(-tidyselect::one_of(id))
  }
  
  grpdat <- select_num_grp(.data, between)
  grpvars <- dplyr::group_vars(grpdat)
  numvars <- names(grpdat)[!names(grpdat) %in% grpvars]
  
  descriptives <- dplyr::bind_rows(
    dplyr::summarise_all(grpdat, ~mean(.)) %>% dplyr::mutate(stat = "mean"),
    dplyr::summarise_all(grpdat, ~stats::sd(.)) %>% dplyr::mutate(stat = "sd"),
    dplyr::summarise_all(grpdat, ~dplyr::n()) %>% dplyr::mutate(stat = "n")
  ) %>%
    tidyr::gather(var, val, dplyr::one_of(numvars)) %>%
    dplyr::mutate(val = round(val, digits)) %>%
    tidyr::spread(stat, val)
  
  stats <- grpdat %>%
    tidyr::nest(tidyselect::one_of(numvars), .key = "multisim_data") %>%
    dplyr::mutate(multisim_cor = purrr::map(multisim_data, function(d) {
      cor(d) %>% round(digits) %>% tibble::as_tibble(rownames = "var")
    })) %>%
    dplyr::select(-multisim_data) %>%
    tidyr::unnest(multisim_cor) %>%
    dplyr::left_join(descriptives, by = c(between, "var")) %>%
    dplyr::select(tidyselect::one_of(c(between, "n", "var", numvars, "mean", "sd")))
    
  if (usekable) {
    print(knitr::kable(stats))
  }
  
  invisible(stats)
}

#' @rdname get_params
#' @export
check_sim_stats <- get_params
