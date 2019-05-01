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
#' 

check_sim_stats <- function(dat, within = c(), between = c(), dv = c(), id = c(),
                            digits = 2, usekable = FALSE, grp_by = between) {
  
  if (length(within) && length(dv) && length(id)) {
    # convert long to wide
    dat <- long2wide(dat, within, between, dv, id) %>%
      dplyr::select(-tidyselect::one_of(id))
  }
  
  grpdat <- select_num_grp(dat, grp_by)
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
    dplyr::left_join(descriptives, by = c(grp_by, "var")) %>%
    dplyr::select(tidyselect::one_of(c(grp_by, "n", "var", numvars, "mean", "sd")))
    
  if (usekable) {
    return(knitr::kable(stats))
  } else {
    return(stats)
  }
}


#' Long to wide format
#' 
long2wide <- function(dat, within = c(), between = c(), dv = c(), id = c()) {
  dat %>%
    dplyr::select(tidyselect::one_of(c(between, within, dv, id))) %>%
    tidyr::unite(".tmpwithin.", tidyselect::one_of(within))  %>%
    dplyr::group_by_at(dplyr::vars(tidyselect::one_of(between))) %>%
    tidyr::spread(".tmpwithin.", !!quo(dv)) %>%
    dplyr::ungroup()
}
