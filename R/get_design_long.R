#' Get design from long data
#' 
#' Makes a best guess at the design of a long-format data frame. 
#' Finds all columns that contain a single value per unit of analysis (between factors), 
#' all columns that contain the same values per unit of analysis (within factors), and 
#' all columns that differ over units of analysis (dv, continuous factors)
#' 
#' @param .data the data frame (in long format)
#' @param dv the column name that identifies the DV
#' @param id the column name(s) that identify a unit of analysis
#' @param plot whether to show a plot of the design
#' 
#' @return the data frame in long format
#' 
#' @export
#'
get_design_long <- function(.data, dv = "y", id = "id", plot = TRUE) {
  between_factors <- .data %>%
    dplyr::group_by_at(dplyr::vars(tidyselect::one_of(id))) %>%
    dplyr::summarise_all(dplyr::n_distinct) %>%
    dplyr::ungroup() %>%
    dplyr::select(-tidyselect::one_of(id)) %>%
    dplyr::summarise_all(max) %>%
    dplyr::select_if(~ . == 1) %>%
    names()
  
  within_factors <- .data %>%
    dplyr::select(-tidyselect::one_of(between_factors)) %>%
    dplyr::group_by_at(dplyr::vars(tidyselect::one_of(id))) %>%
    dplyr::summarise_all(paste0, collapse = ",") %>%
    dplyr::ungroup() %>%
    dplyr::select(-tidyselect::one_of(id)) %>%
    dplyr::summarise_all(dplyr::n_distinct) %>%
    dplyr::select_if(~ . == 1) %>%
    names()
  
  within <- .data %>%
    dplyr::select(tidyselect::one_of(within_factors)) %>%
    dplyr::mutate_all(as.factor) %>%
    dplyr::summarise_all(~levels(.) %>% paste0(collapse = ".|.")) %>%
    as.list() %>%
    sapply(strsplit, split=".|.", fixed = TRUE) %>%
    purrr::map(fix_name_labels)
  
  between <- .data %>%
    dplyr::select(tidyselect::one_of(between_factors)) %>%
    dplyr::mutate_all(as.factor) %>%
    dplyr::summarise_all(~levels(.) %>% paste0(collapse = ".|.")) %>%
    as.list() %>%
    sapply(strsplit, split=".|.", fixed = TRUE) %>%
    purrr::map(fix_name_labels)
  
  # define columns
  cells_w <- cell_combos(within, dv)
  cells_b <- cell_combos(between, dv) 
  
  # get n, mu, sd, r per cell
  chk <- check_sim_stats(.data, between_factors, within_factors, dv, id)
  
  if (length(between_factors)) {
    chk_b <- tidyr::unite(chk, ".between", tidyselect::one_of(between_factors)) %>%
      dplyr::mutate(.between = forcats::fct_relevel(.between, cells_b)) %>%
      dplyr::arrange(.between)
  } else {
    chk_b <- dplyr::mutate(chk, ".between" = dv)
  }
  
  n <- chk_b %>%
    dplyr::select(.between, var, n) %>%
    tidyr::spread(var, n) %>%
    dplyr::select(tidyselect::one_of(c(".between", cells_w))) %>%
    tibble::column_to_rownames(".between") %>% 
    as.data.frame()
  
  mu <- chk_b %>%
    dplyr::select(.between, var, mean) %>%
    tidyr::spread(var, mean) %>%
    dplyr::select(tidyselect::one_of(c(".between", cells_w))) %>%
    tibble::column_to_rownames(".between") %>% 
    as.data.frame()
  
  sd <- chk_b %>%
    dplyr::select(.between, var, sd) %>%
    tidyr::spread(var, sd) %>%
    dplyr::select(tidyselect::one_of(c(".between", cells_w))) %>%
    tibble::column_to_rownames(".between") %>% 
    as.data.frame()
  
  cors <- chk_b %>%
    dplyr::select(tidyselect::one_of(c(".between", "var", cells_w))) %>%
    dplyr::mutate(var = forcats::fct_relevel(var, cells_w)) %>%
    dplyr::arrange(var) %>%
    dplyr::group_by(.between) %>%
    tidyr::nest(.key = "r") %>%
    as.list() 
  
  r <- purrr::map(cors$r, ~tibble::column_to_rownames(., "var") %>% as.matrix())
  names(r) <- cors$.between
  
  check_design(within, between, n, mu, sd, r, dv, id, plot)
}

