#' Get parameters from a data table
#'
#' Generates a table of the correlations and means of numeric columns in a data frame. If data was generated by \code{sim_design} and has a "design" attribute, between, within, dv and id are retrieved from that, unless overridden (use between = 0 to 
#'
#' @param data the existing tbl
#' @param between a vector of column names for between-subject factors
#' @param within a vector of column names for within-subject factors (if data is long)
#' @param dv the column name(s) of the dv, if NULL all numeric columns will be selected
#' @param id the column name(s) of the subject ID, excluded from the table even if numeric
#' @param digits how many digits to round to (default = 2)
#' 
#' @return a tbl of correlations, means and sds
#' @examples
#' get_params(iris, "Species")
#' @export
#' 

get_params <- function(data, between = NULL, within = NULL, 
                       dv = NULL, id = NULL, digits = 2) {
  # error checking -----------------
  if (is.matrix(data)) {
    data <- as.data.frame(data)
  } else if (!is.data.frame(data)) {
    stop("data must be a data frame or matrix")
  }
  
  # get between/within from design if not specified
  design <- get_design(data)
  if (!is.null(design)) {
    if (is.null(between)) between <- names(design$between)
    if (is.null(within)) within <- names(design$within)
    if (is.null(dv)) dv <- names(design$dv)
    if (is.null(id)) id <- names(design$id)
  }
  
  if (is.numeric(between)) between <- names(data)[between]
  
  if (length(between) > 0 && !is.character(between)) {
    stop("between must be a numeric or character vector")
  }
  
  # convert long to wide if right attributes specified
  # and consistent with data
  if (length(within) && !is.null(dv) && !is.null(id) &&
      all(c(within, dv, id) %in% names(data))) {
    data <- long2wide(data, within, between, dv, id)
    #data$id <- NULL
  }
  
  # get all numeric DVs in the dataset
  numvars <- setdiff(names(data), c(id, between))
  is_num <- sapply(data[numvars], is.numeric)
  numvars <- numvars[is_num]
  if (!is.null(dv) && all(dv %in% numvars)) numvars <- dv
  
  grps <- data[between]
  if (length(grps) == 0) grps <- rep(1, nrow(data))
  
  desc <- by(data, grps, function(x) {
    m <- apply(x[numvars], 2, mean)
    sd <- apply(x[numvars], 2, sd)
    desc <- data.frame(var = names(m),
                       mean = round(m, digits), 
                       n = nrow(x), 
                       sd = round(sd, digits))
    for (b in between) desc[b] <- unique(x[[b]])
    desc[, c(between, "var", "mean", "n", "sd")]
  })
  
  desc <- do.call(rbind, desc)
  desc$var <- factor(desc$var, numvars)
  ord <- do.call(order, desc[c(between, "var")])
  descriptives <- desc[ord, ]
  
  if (length(numvars) > 1) {
    r <- by(data, grps, function(x) {
      r <- cor(x[numvars]) %>% round(digits) %>% as.data.frame()
      r$var <- row.names(r)
      for (b in between) r[b] <- unique(x[b])
      r
    })
    
    r <- do.call(rbind, r)
    r$var <- factor(r$var, numvars)
    ord <- do.call(order, r[c(between, "var")])
    r <- r[ord, ]
    
    descriptives[c(between, "var")] <- NULL
    stats <- cbind(r, descriptives)
    var_order <- c(between, "n", "var", numvars, "mean", "sd")
  } else {
    stats <- descriptives
    var_order <- c(between, "n", "mean", "sd")
  }
  stats <- stats[, var_order]
  row.names(stats) <- NULL
  
  stats
}

#' @rdname get_params
#' @export
check_sim_stats <- get_params
