#' Add a dependent variable to a mixed model simulation
#'
#' @param .data the data frame
#' @param formula The formula for your model
#'
#' @return a data frame with new DV column
#' @export
#'
#' @examples
#' add_random(rater = 5) |> add_dv()
add_dv <- function(.data, formula = NULL, 
                   dv_name = "y",
                   intercept = 0) {
  formula = "y ~ (B1 * W1) + (age) + (1 + c | x)"
  if (is.null(formula)) {
    formula <- paste0(dv_name, "~ 1") |> stats::as.formula()
  } else if (is.character(formula)) {
    formula <- stats::as.formula(formula)
  }
  
  terms <- stats::terms.formula(formula)
  ivs <- attr(terms, "term.labels")
  ivs <- ivs[!grepl("\\|", ivs)]
  dv <- as.character(formula[[2]])
  .data[dv] <- 0
  if (attr(terms, "intercept")) {
    .data[dv] <- .data[dv] + intercept
  }
  
  return(.data)
}
