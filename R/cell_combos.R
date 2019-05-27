#' Cell combos
#' 
#' Creates wide cell combination names, such as A1_B1, A2_B1, A1_B2, A2_B2.
#' 
#' @param factors A list of lists of named factor levels
#' @param dv name of dv column ("y") to be used if there are no factors
#' 
#' @return a list
#' @keywords internal
#' 
#' @examples 
#' 
#' factors <- list(
#'   speed = c(fast = "Fast Condition", slow = "Slow Condition"),
#'   condition = c(A = "Condition A", B = "Condition B")
#' )
#' faux:::cell_combos(factors)
#' 
cell_combos <- function(factors, dv = "y") {
  if (length(factors) == 0) {
    cells = dv
  } else {
    cells <- purrr::map(factors, ~{factor(names(.), levels = names(.))}) %>%
      do.call(tidyr::crossing, .) %>%
      #do.call(expand.grid, .) %>%
      tidyr::unite("b", 1:ncol(.)) %>% 
      dplyr::pull("b")
  }
  
  cells
}
