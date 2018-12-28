#' Check table stats
#'
#' \code{check_sim_stats} Generates a table of the correlations and means of numeric columns in a data frame
#'
#' @param dat the existing dataframe
#' @param grp_by an optional list of column names to group by
#' @param digits how many digits to round to (default = 2)
#' @param usekable logical. If TRUE, output with knitr::kable
#' 
#' @return tibble or kable
#' @examples
#' check_sim_stats(iris, "Species")
#' @export

check_sim_stats <- function(dat, grp_by = NULL, digits = 2, usekable = FALSE) {
  grpdat <- select_num_grp(dat, grp_by)
  grpvars <- dplyr::group_vars(grpdat)
  numvars <- names(grpdat)[!names(grpdat) %in% grpvars]
  
  descriptives <- dplyr::bind_rows(
    dplyr::summarise_all(grpdat, mean) %>% dplyr::mutate(stat = "mean"),
    dplyr::summarise_all(grpdat, sd) %>% dplyr::mutate(stat = "sd")
  ) %>%
    tidyr::gather(var, val, dplyr::one_of(numvars)) %>%
    dplyr::mutate(val = round(val, digits)) %>%
    tidyr::spread(stat, val)
  
  stats <- grpdat %>%
    tidyr::nest(dplyr::one_of(numvars), .key = "multisim_data") %>%
    dplyr::mutate(multisim_cor = purrr::map(multisim_data, function(d) {
      cor(d) %>% round(digits) %>% tibble::as_tibble(rownames = "var")
    })) %>%
    dplyr::select(-multisim_data) %>%
    tidyr::unnest(multisim_cor) %>%
    dplyr::left_join(descriptives, by = c(grp_by, "var"))
    
  if (usekable) {
    return(knitr::kable(stats))
  } else {
    return(stats)
  }
}
