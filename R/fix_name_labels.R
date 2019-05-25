#' Fix name labels
#' 
#' Fixes if a factor list does not have named levels or has special characters in the names
#' 
#' @param x the list to fix
#' @param replace regex pattern to replace with full stops (defaults to non-word characters and underscores)
#' 
#' @return the fixed list
#' @keywords internal
#' 
fix_name_labels <- function(x, replace = "(\\W|_)") {
  if (is.null(names(x))) { names(x) <- x }
  nm <- names(x)
  # replace non-word characters and underscores with full stops
  names(x) <- gsub(replace, ".", nm) 
  as.list(x)
}
