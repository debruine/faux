#' Generate a sample with random intercepts for subjects and items
#'
#' \code{sim_mixed_df()} produces a data table with the same distributions of by-subject and by-item random intercepts as an existing data table.
#'
#' @param .data the existing tbl
#' @param sub_n the number of subjects to simulate
#' @param item_n the number of items to simulate
#' @param dv the column name or index containing the DV
#' @param sub_id the column name or index for the subject IDs
#' @param item_id the column name or index for the item IDs
#' 
#' @return a tbl
#' @examples
#' \donttest{sim_mixed_df(faceratings, 10, 10, "rating", "rater_id", "face_id")}
#' @export

sim_mixed_df <- function(.data, sub_n = 100, item_n = 25, 
                        dv = 1, sub_id = 2, item_id = 3) {
  # error checking -------------------------------------------------------------
  if (is.matrix(.data)) {
    .data = as.data.frame(.data)
  } else if (!is.data.frame(.data)) {
    stop(".data must be a data frame or matrix")
  }
  
  # get column names if specified by index
  if (is.numeric(dv)) dv <- names(.data)[dv]
  if (is.numeric(sub_id)) sub_id <- names(.data)[sub_id]
  if (is.numeric(item_id)) item_id <- names(.data)[item_id]
  
  lmer_formula <- paste0(dv, " ~ 1 + (1 | ", sub_id, ") + (1 | ", item_id, ")") %>%
    stats::as.formula()
  mod <- lme4::lmer(lmer_formula, data = .data)
  grand_i <- lme4::fixef(mod)
  
  sds <- lme4::VarCorr(mod) %>% as.data.frame()
  sub_sd <- dplyr::filter(sds, grp == sub_id) %>% dplyr::pull(sdcor)
  item_sd <- dplyr::filter(sds, grp == item_id) %>% dplyr::pull(sdcor)
  error_sd <- dplyr::filter(sds, grp == "Residual") %>% dplyr::pull(sdcor)
  
  new_obs <- sim_mixed_cc(sub_n, item_n, grand_i, sub_sd, item_sd, error_sd)
  
  new_obs
}