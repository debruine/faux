#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL



## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))


#' Print Codebook Object
#'
#' @param x The psychds_codebook list
#' @param ... Additional parameters for print
#'
#' @export
#'
print.psychds_codebook <- function(x, ...) {
  
  if ("Psych-DS 0.1.0" == x$schemaVersion &
      length(x$variableMeasured) > 0) {
    
    cat("Codebook for", x$name, "(Psych-DS 0.1.0)\n")
    
    vars <- list()
    for (v in x$variableMeasured) {
      desc <- ifelse(v$name == v$description,
                     "", paste(":", v$description))
      extras <- ""
      if (v$type == "factor") {
        lvls <- ""
        if (length(v$levels) > 0) {
          if (all(names(v$levels) == v$levels)) {
            l <- names(v$levels)
          } else {
            l <- paste0(names(v$levels), ": ", v$levels)
          }
          lvls <- sprintf("    * %s\n",
                          paste(l, collapse = "\n    * "))
        }
        
        extras <- sprintf("\n  * Levels\n%s  * Ordered: %s",
                          lvls,
                          ifelse(is.null(v$ordered), FALSE, v$ordered)
        )
      }
      vars[v$name] = sprintf(
        "* %s (%s)%s%s",
        v$name, v$type, desc, extras
      )
    }
    
    paste(vars, collapse = "\n") %>%cat()
  } else {
    utils::str(x)
  }
}

