#' Generate a cross-classified sample 
#' 
#' Makes a basic cross-classified design with random intercepts for subjects and items
#'
#' @param sub_n the number of subjects
#' @param item_n the number of items
#' @param grand_i the grand intercept (overall mean)
#' @param sub_sd the SD of subject random intercepts
#' @param item_sd the SD of item random intercepts
#' @param error_sd the SD of the error term
#' @param seed a single value, interpreted as an integer, or NULL (see set.seed)
#'
#' @return a tbl 
#' @export
#'
#' @examples
#' 
#' sim_mixed_cc(10, 10)
sim_mixed_cc <- function(sub_n = 100, item_n = 20, grand_i = 0, 
                         sub_sd = 1, item_sd = 1, error_sd = 1, seed = NULL) {
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
  
  # sample subject random intercepts -------------------------------------------
  if (length(sub_sd) == sub_n) {
    # use exact subject intercepts and names
    new_sub <- data.frame(
      sub_id = rownames(sub_sd),
      sub_i = sub_sd %>% unname()
    )
  } else if (length(sub_sd) == 1) {
    # sample new subjects and intercepts
    new_sub <- data.frame(
      sub_id = make_id(1:sub_n, "S"),
      sub_i = stats::rnorm(sub_n, 0, sub_sd)
    )
  }
  
  
  # sample item random intercepts ----------------------------------------------
  if (length(item_sd) == item_n) {
    # use exact item intercepts and names
    new_item <- data.frame(
      item_id = rownames(item_sd),
      item_i = item_sd %>% unname()
    )
  } else if (length(item_sd) == 1) {
    # sample new items and intercepts
    new_item <- data.frame(
      item_id = make_id(1:item_n, "I"),
      item_i = stats::rnorm(item_n, 0, item_sd)
    )
  }
  
  new_obs <- expand.grid(
    sub_id = new_sub$sub_id,
    item_id = new_item$item_id
  ) %>%
    dplyr::left_join(new_sub, by = "sub_id") %>%
    dplyr::left_join(new_item, by = "item_id") %>%
    dplyr::mutate(
      grand_i = grand_i,
      err = stats::rnorm(nrow(.), 0, error_sd),
      y = grand_i + sub_i + item_i + err
    ) %>%
    dplyr::select(sub_id, item_id, y, grand_i, sub_i, item_i, err)
  
  new_obs
}
