#' Make ID
#' 
#' Make IDs with fixed length and a letter prefix for random effects (e.g., S001, S002, ..., S100).
#' @param n the number of IDs to generate
#' @param prefix the letter prefix to the number
#' 
#' @return a vector of IDs
#' @export
#' 
#' @examples 
#' 
#' make_id(20, "SUBJECT_")
#' 
make_id <- function(n = 100, prefix = "S") {
  max_digits <- floor(log10(n))+1
  paste0(prefix, formatC(1:n, width = max_digits, flag = "0"))
}