#' Select grouping and numeric columns and group
#'
#' \code{select_num_grp} Select grouping and numeric columns and group
#'
#' @param dat the existing dataframe
#' @param grp_by an optional list of column names to group by
#' 
#' @return tibble
#' @examples
#' select_num_grp(iris, "Species")
#' @export

select_num_grp <- function(dat, grp_by = NULL) {
  # error checking -----------------
  if (is.matrix(dat)) {
    dat = as.data.frame(dat)
  } else if (!is.data.frame(dat)) {
    stop("dat must be a data frame or matrix")
  }
  
  # select only grouping and numeric columns -----------------
  if (is.null(grp_by)) {
    # no grouping, so select all numeric columns
    numdat <- dplyr::select_if(dat, is.numeric)
    grpdat <- numdat
  } else if (is.numeric(grp_by) || is.character(grp_by)) {
    # get grouping column names if specified by index
    if (is.numeric(grp_by)) grp_by <- names(dat)[grp_by]
    
    # numeric columns, exclusing grouping columns
    numdat <- dat %>%
      dplyr::select(-dplyr::one_of(grp_by)) %>%
      dplyr::select_if(is.numeric)
    
    # get grouping columns, add remaining numeric columns, and group
    grpdat <- dat %>%
      dplyr::select(dplyr::one_of(grp_by)) %>%
      dplyr::bind_cols(numdat) %>%
      dplyr::group_by_at(dplyr::vars(dplyr::one_of(grp_by)))
  } else {
    stop("grp_by must be a numeric or character vector")
  }
  
  return(grpdat) 
}
