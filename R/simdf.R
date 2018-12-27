#' Simulate an existing dataframe
#'
#' \code{simdf} Produces a dataframe with the same distributions and correlations as an existing dataframe. Only returns numeric columns and simulates all numeric variables from a continuous normal distribution (for now).
#'
#' @param dat the existing dataframe
#' @param n the number of samples to return per group
#' @param grp_by an optional list of column names to group by
#' 
#' @return tibble
#' @examples
#' iris100 <- simdf(iris, 100)
#' iris_species <- simdf(iris, 100, "Species")
#' @export

simdf <- function (dat, n=100, grp_by=NULL) {
  numdat <- dplyr::select_if(dat, is.numeric)
  if (!is.null(grp_by)) {
    grpdat <- dat %>%
      dplyr::select(dplyr::one_of(grp_by)) %>%
      dplyr::bind_cols(numdat) %>%
      dplyr::group_by_at(dplyr::vars(dplyr::one_of(grp_by)))
  } else {
    grpdat <- numdat
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
        varnames = names(data)
      )
    })) %>%
    dplyr::select(-data) %>%
    tidyr::unnest(newsim) %>%
    dplyr::ungroup()
  
  return(simdat)
}
