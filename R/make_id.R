#' Make ID
#' 
#' Make IDs with fixed length and a letter prefix for random effects (e.g., S001, S002, ..., S100).
#' @param n the number of IDs to generate (or a vector of numbers)
#' @param prefix the letter prefix to the number
#' @param digits the number of digits to use for the numeric part. Only used if this is larger than the number of digits in n.
#' 
#' @return a vector of IDs
#' @export
#' 
#' @examples 
#' 
#' make_id(20, "SUBJECT_")
#' make_id(10:30, digits = 3)
#' 
make_id <- function(n = 100, prefix = "S", digits = 0) {
  # set max digits to the larger of digits in `n`` or `digits`
  if (length(n) == 1) {
    max_n <- n
    n <- 1:max_n
  } else {
    max_n <- max(n)
  }
  
  max_digits <- max(floor(log10(max_n))+1, digits)
  paste0(prefix, formatC(n, width = max_digits, flag = "0"))
}
