#' Simulate an existing dataframe
#'
#' \code{simdf} Produces a dataframe with the same distributions and correlations as an existing dataframe. Only returns numeric columns and simulates all numeric variables from a continuous normal distribution (for now).
#'
#' @param dat the existing dataframe
#' @param n the number of samples to return per group
#' @param grp_by an optional list of column names to group by
#' @param empirical logical. Passed on to multirnorm
#' 
#' @return tibble
#' @examples
#' iris100 <- simdf(iris, 100)
#' iris_species <- simdf(iris, 100, "Species")
#' @export

simdf <- function (dat, n=100, grp_by=NULL, empirical = FALSE) {
  # error checking
  if (is.matrix(dat)) {
    dat = as.data.frame(dat)
  } else if (!is.data.frame(dat)) {
    stop("dat must be a data frame or matrix")
  }
  
  if ( !is.numeric(n) || n %% 1 > 0 || n < 3 ) {
    stop("n must be an integer > 2")
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
  
  simdat <- grpdat %>%
    tidyr::nest() %>%
    dplyr::mutate(newsim = purrr::map(data, function(data) {
      multirnorm(
        n = n, 
        vars = ncol(data), 
        cor = stats::cor(data),
        mu = t(dplyr::summarise_all(data, mean)), 
        sd = t(dplyr::summarise_all(data, stats::sd)), 
        varnames = names(data),
        empirical = empirical
      )
    })) %>%
    dplyr::select(-data) %>%
    tidyr::unnest(newsim) %>%
    dplyr::ungroup()
  
  return(simdat)
}
