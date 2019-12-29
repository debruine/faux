#' Generate a cross-classified sample 
#' 
#' Makes a basic cross-classified design with random intercepts for subjects and items
#'
#' @param sub_n the number of subjects
#' @param item_n the number of items
#' @param grand_i the grand intercept (overall mean)
#' @param sub_sd the SD of subject random intercepts (or a sub_n-length named vector of random intercepts for each subject)
#' @param item_sd the SD of item random intercepts (or an item_n-length named vector of random intercepts for each item)
#' @param error_sd the SD of the error term
#' @param empirical Should the returned data have these exact parameters? (versus be sampled from a population with these parameters)
#' @param seed a single value, interpreted as an integer, or NULL (see set.seed)
#'
#' @return a tbl 
#' @export
#'
#' @examples
#' 
#' sim_mixed_cc(10, 10)
sim_mixed_cc <- function(sub_n = 100, item_n = 20, grand_i = 0, 
                         sub_sd = 1, item_sd = 1, error_sd = 1, 
                         empirical = FALSE, seed = NULL) {
  if (!is.null(seed)) {
    # reinstate system seed after simulation
    sysSeed <- .GlobalEnv$.Random.seed
    on.exit({
      if (!is.null(sysSeed)) {
        .GlobalEnv$.Random.seed <- sysSeed 
      } else {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    })
    set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion")
  }
  
  # sample subject random intercepts----
  if (length(sub_sd) == sub_n) {
    # use exact subject intercepts and names
    sub_id <- names(sub_sd) # if sub_sd is a named vector or list
    if (is.null(sub_id)) sub_id <- make_id(1:sub_n, "S")
    
    new_sub <- data.frame(
      sub_id = sub_id,
      sub_i = sub_sd %>% unname()
    )
  } else if (length(sub_sd) == 1) {
    # sample new subjects and intercepts
    new_sub <- data.frame(
      sub_id = make_id(1:sub_n, "S"),
      sub_i = rnorm_multi(sub_n, 1, 0, sub_sd, 
                          empirical = empirical, 
                          as.matrix = TRUE)[,1]
    )
  }
  
  
  # sample item random intercepts ----
  if (length(item_sd) == item_n) {
    # use exact item intercepts and names
    item_id <- names(item_sd) # if item_sd is a named vector or list
    if (is.null(item_id)) item_id <- make_id(1:item_n, "I")
    
    new_item <- data.frame(
      item_id = item_id,
      item_i = item_sd %>% unname()
    )
  } else if (length(item_sd) == 1) {
    # sample new items and intercepts
    new_item <- data.frame(
      item_id = make_id(1:item_n, "I"),
      item_i = rnorm_multi(item_n, 1, 0, item_sd, 
                           empirical = empirical, 
                           as.matrix = TRUE)[,1]
    )
  }
  
  new_obs <- tidyr::crossing(
    sub_id = new_sub$sub_id,
    item_id = new_item$item_id
  ) %>%
    dplyr::left_join(new_sub, by = "sub_id") %>%
    dplyr::left_join(new_item, by = "item_id") %>%
    dplyr::mutate(
      "grand_i" = grand_i,
      "err" = stats::rnorm(nrow(.), 0, error_sd),
      "y" = .data$grand_i + .data$sub_i + .data$item_i + .data$err
    ) %>%
    dplyr::select(.data$sub_id, .data$item_id, 
                  .data$y, .data$grand_i, 
                  .data$sub_i, .data$item_i, .data$err)
  
  new_obs
}
