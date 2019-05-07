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
#'
#' @return a tbl 
#' @export
#'
#' @examples
#' 
#' sim_mixed_cc(10, 10)
sim_mixed_cc <- function(sub_n = 100, item_n = 20, grand_i = 0, sub_sd = 1, item_sd = 1, error_sd = 1) {
  # sample subject random intercepts -------------------------------------------
  new_sub <- data.frame(
    sub_id = make_id(1:sub_n, "S"),
    sub_i = stats::rnorm(sub_n, 0, sub_sd)
  )
  
  # sample item random intercepts ----------------------------------------------
  new_item <- data.frame(
    item_id = make_id(1:item_n, "I"),
    item_i = stats::rnorm(item_n, 0, item_sd)
  )
  
  new_obs <- expand.grid(
    sub_id = new_sub$sub_id,
    item_id = new_item$item_id
  ) %>%
    dplyr::left_join(new_sub, by = "sub_id") %>%
    dplyr::left_join(new_item, by = "item_id") %>%
    dplyr::mutate(
      err = stats::rnorm(nrow(.), 0, error_sd),
      val = grand_i + sub_i + item_i + err
    ) %>%
    dplyr::select(sub_id, item_id, val)
  
  new_obs
}
