#' Simulate category joint distribution
#'
#' @param data the existing tbl
#' @param ... columns to calculate the joint distribution from, if none are chose, all columns with 10 or fewer unique values will be chosen
#' @param n the number of total observations to return
#' @param empirical Should the returned data have the exact same distribution of conditions? (versus be sampled from a population with this distribution)
#'
#' @return data table
#' @export
#'
#' @examples
#' sim_joint_dist(mtcars, cyl, vs, am, n = 20)
sim_joint_dist <- function(data, ..., n = 100, empirical = FALSE) {
  cols <- getcols(data, ...)
  # if not specified, choose all columns with < 10 values
  if (length(cols) == 0) {
    col_unique <- lapply(data, unique) %>%
      sapply(length)

    cols <- names(data[, col_unique<=10, drop = FALSE])
  }

  # count instances of each unique combo
  grps <- data[cols]
  combos <- by(data, grps, function(x) {
    y <- x[1, cols, drop = FALSE]
    y$.n. <- nrow(x)
    y
  }) %>% do.call(rbind, .)
  
  # sample n combos with probs from above
  prob <- combos$.n.
  if (empirical == TRUE) {
    each <- round(n*prob/sum(prob))
    diff <- n - sum(each) # because of rounding, total is sometimes not n
    each[1] <- each[1] + diff
    samp <- rep(1:nrow(combos), each)
  } else {
    samp <- sample(1:nrow(combos), n, TRUE, prob)
  }
  
  # make a new data from with sampled combo columns
  combos$.n. <- NULL
  new_data <- lapply(samp, function(x) {
    combos[x, , drop = FALSE]
  }) %>% do.call(rbind, .)
  
  rownames(new_data) <- c()
  
  new_data
}

