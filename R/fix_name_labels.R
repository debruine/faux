#' Fix name labels
#' 
#' Fixes if a factor list does not have named levels or has special characters in the names
#' 
#' @param x the vector or list to fix
#' @param pattern regex pattern to replace with full stops (defaults to non-word characters and underscores)
#' @param replacement the character to replace (defaults to .)
#' 
#' @return a named list with fixed names
#' @export
#' 
#' @examples 
#' source <- list("full.stop", " space ", "under_score", "plus+", "dash-", "tab\t", "line\nbreak")
#' fix_name_labels(source)
#' 
fix_name_labels <- function(x, pattern = "(\\W|_)", replacement = ".") {
  if (!is.list(x) & !is.vector(x)) stop("x must be a vector or list")
  
  if (is.null(names(x))) { names(x) <- x }
  nm <- names(x)
  if (!is.null(pattern)) {
    # replace non-word characters and underscores with full stops
    names(x) <- gsub(pattern = pattern, replacement = replacement, x = nm) 
  }
  as.list(x)
}
