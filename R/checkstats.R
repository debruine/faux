#' Check table stats
#'
#' \code{checkstats} Generates a table of the correlations and means of numeric columns in a data frame
#'
#' @param dat the existing dataframe
#' @param grp_by an optional list of column names to group by
#' @param digits how many digits to round to (default = 2)
#' @param usekable logical. If TRUE, output with knitr::kable
#' 
#' @return tibble or kable
#' @examples
#' checkstats(iris, "Species")
#' @export

checkstats <- function(dat, grp_by = NULL, digits = 2, usekable = FALSE) {
  # error checking
  if (is.matrix(dat)) {
    dat = as.data.frame(dat)
  } else if (!is.data.frame(dat)) {
    stop("dat must be a data frame or matrix")
  }
  
  if (is.null(grp_by)) {
    numdat <- dplyr::select_if(dat, is.numeric)
    grpdat <- numdat
  } else if (is.numeric(grp_by) || is.character(grp_by)) {
    if (is.numeric(grp_by)) grp_by <- names(dat)[grp_by]
    
    numdat <- dat %>%
      dplyr::select(-dplyr::one_of(grp_by)) %>%
      dplyr::select_if(is.numeric)
    grpdat <- dat %>%
      dplyr::select(dplyr::one_of(grp_by)) %>%
      dplyr::bind_cols(numdat) %>%
      dplyr::group_by_at(dplyr::vars(dplyr::one_of(grp_by)))
  } else {
    stop("grp_by must be a numeric or character vector")
  }
  
  stats <- dplyr::bind_rows(
    dplyr::summarise_all(grpdat, mean) %>% dplyr::mutate(stat = "mean"),
    dplyr::summarise_all(grpdat, sd) %>% dplyr::mutate(stat = "sd")
  ) %>%
    tidyr::gather(var, val, dplyr::one_of(names(numdat))) %>%
    dplyr::mutate(val = round(val, digits)) %>%
    tidyr::spread(stat, val)
  
  cors <- grpdat %>%
    tidyr::nest(dplyr::one_of(names(numdat)), .key = "multisim_data") %>%
    dplyr::mutate(multisim_cor = purrr::map(multisim_data, function(d) {
      cor(d) %>% round(digits) %>% tibble::as_tibble(rownames = "var")
    })) %>%
    dplyr::select(-multisim_data) %>%
    tidyr::unnest(multisim_cor) %>%
    dplyr::left_join(stats, by = c(grp_by, "var"))
    
  if (usekable) {
    return(knitr::kable(cors))
  } else {
    return(cors)
  }
}
