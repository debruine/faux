#' Select grouping and numeric columns and group
#'
#' \code{select_num_grp} Select grouping and (optionally specified) numeric columns and group
#'
#' @param .data the existing tbl
#' @param between an optional list of column names to group by
#' @param cols an optional list of column names to return (default of NULL returns all numeric columns)
#' 
#' @return a tbl
#' @examples
#' select_num_grp(iris, "Species")
#' @export

select_num_grp <- function(.data, between = c(), cols = NULL) {
  # error checking -----------------
  if (is.matrix(.data)) {
    .data = as.data.frame(.data)
  } else if (!is.data.frame(.data)) {
    stop(".data must be a data frame or matrix")
  }
  
  # select only grouping and numeric columns -----------------
  if (is.null(between)) {
    # no grouping, so select all numeric columns
    numdat <- dplyr::select_if(.data, is.numeric)
    grpdat <- numdat
  } else if (is.numeric(between) || is.character(between)) {
    # get grouping column names if specified by index
    if (is.numeric(between)) between <- names(.data)[between]
    
    # numeric columns, excluding grouping columns
    numdat <- .data %>%
      dplyr::select(-tidyselect::one_of(between)) %>%
      dplyr::select_if(is.numeric)
    
    # get grouping columns, add remaining numeric columns, and group
    grpdat <- .data %>%
      dplyr::select(tidyselect::one_of(between)) %>%
      dplyr::bind_cols(numdat) %>%
      dplyr::group_by_at(dplyr::vars(tidyselect::one_of(between)))
  } else {
    stop("between must be a numeric or character vector")
  }
  
  if (!is.null(cols)) {
    # return only grouping and cols
    if (is.numeric(cols)) cols <- names(.data)[cols]
    
    grpdat <- grpdat %>%
      dplyr::select(tidyselect::one_of(c(between, cols)))
  }
  
  return(grpdat) 
}
