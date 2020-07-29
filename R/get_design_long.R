#' Get design from long data
#' 
#' Makes a best guess at the design of a long-format data frame. 
#' Finds all columns that contain a single value per unit of analysis (between factors), 
#' all columns that contain the same values per unit of analysis (within factors), and 
#' all columns that differ over units of analysis (dv, continuous factors)
#' 
#' @param data the data frame (in long format)
#' @param dv the column name that identifies the DV
#' @param id the column name(s) that identify a unit of analysis
#' @param plot whether to show a plot of the design
#' 
#' @return the data frame in long format
#' 
#' @export
#'
get_design_long <- function(data, dv = "y", id = "id", plot = faux_options("plot")) {
  data <- dplyr::ungroup(data)
  
  cnames <- setdiff(names(data), c(id, dv))
  
  # check for columns where there is only ever one value per id
  y <- by(data, data[id], function(x) {
    unique_vals <- lapply(x, unique)
    n_unique_vals <- lapply(unique_vals, length)
    as.data.frame(n_unique_vals)
  })
  
  z <- do.call(rbind, y)
  factors <- lapply(z, function(x) {
    ifelse(max(x) > 1, "W", "B")
  })
  # get rid of id and dv columns
  factors <- factors[names(factors) %in% cnames]
  
  between_factors <- names(factors[which(factors == "B")])
  within_factors <- names(factors[which(factors == "W")])
  
  # get levels for each column
  lvls <- data[, names(data) %in% cnames, drop = FALSE]
  lvls <- lapply(lvls, unique)
  lvls <- lapply(lvls, as.character)
  lvls <- lapply(lvls, fix_name_labels)
  
  within <- lvls[which(names(lvls) %in% within_factors)]
  between <- lvls[which(names(lvls) %in% between_factors)]
  
  # define columns
  cells_w <- cell_combos(within, dv)
  cells_b <- cell_combos(between, dv) 
  
  # get n, mu, sd, r per cell
  chk <- check_sim_stats(data, between_factors, within_factors, dv, id, digits = 8)
  
  if (length(between_factors)) {
    chk_b <- tidyr::unite(chk, ".between", tidyselect::one_of(between_factors)) %>%
      dplyr::mutate(".between" = forcats::fct_relevel(.data$.between, cells_b)) %>%
      dplyr::arrange(.data$.between)
  } else {
    chk_b <- dplyr::mutate(chk, ".between" = dv)
  }
  
  n <- chk_b %>%
    dplyr::select(.data$.between, .data$var, .data$n) %>%
    tidyr::spread(.data$var, .data$n) %>%
    dplyr::select(tidyselect::one_of(c(".between", cells_w))) %>%
    tibble::column_to_rownames(".between") %>% 
    as.data.frame()
  
  mu <- chk_b %>%
    dplyr::select(.data$.between, .data$var, .data$mean) %>%
    tidyr::spread(.data$var, .data$mean) %>%
    dplyr::select(tidyselect::one_of(c(".between", cells_w))) %>%
    tibble::column_to_rownames(".between") %>% 
    as.data.frame()
  
  sd <- chk_b %>%
    dplyr::select(.data$.between, .data$var, .data$sd) %>%
    tidyr::spread(.data$var, .data$sd) %>%
    dplyr::select(tidyselect::one_of(c(".between", cells_w))) %>%
    tibble::column_to_rownames(".between") %>% 
    as.data.frame()
  
  cors <- chk_b %>%
    dplyr::select(tidyselect::one_of(c(".between", "var", cells_w))) %>%
    dplyr::mutate("var" = forcats::fct_relevel(.data$var, cells_w)) %>%
    dplyr::arrange(.data$var) %>%
    dplyr::group_by(.data$.between) %>%
    tidyr::nest("r" = -.data$.between) %>%
    as.list() 
  
  r <- purrr::map(cors$r, ~tibble::column_to_rownames(., "var") %>% as.matrix())
  names(r) <- cors$.between
  
  check_design(within, between, n, mu, sd, r, dv, id, plot)
}

