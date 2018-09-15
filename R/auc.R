#' Combine all runs on a day / case by simple arithmetic mean
#'
#'
#' @param df Dataframe holding nano data
#' @param mean_var column holding to aggregate to mean
#' @param ... variables to aggregate on
#'
#' @export
nano_means <- function(df, mean_var = count, ...){
  count <- `:=` <-  NULL # to survive CRAN checks
  mean_var <- rlang::enquo(mean_var)
  result_var <- rlang::sym(paste0(rlang::quo_text(mean_var), "_mean"))
  group_vars <- rlang::enquos(...)
  df %>%
  dplyr::group_by_at(dplyr::vars(!!!group_vars)) %>%
  dplyr::summarise(!!result_var := mean(!!mean_var, na.rm = TRUE)) %>%
  dplyr::ungroup()
}
