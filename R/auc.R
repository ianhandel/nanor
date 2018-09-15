#' Combine all runs on a day / case by simple arithmetic mean
#'
#'
#' @param df Dataframe holding nano data
#' @param count_vat column holding particle counts
#'
#' @export
nano_means <- function(df, count_var = count){
  count_var <- dplyr::enquo(count_var)
  df %>%
  dplyr::group_by_at(dplyr::vars(-!!count_var)) %>%
  dplyr::summarise(mean_count = mean(!!count_var, na.rm = TRUE)) %>%
  dplyr::ungroup()
}

# adds AUC (sum count)
add_auc <- function(df){
  df %>%
  mutate(AUC = sum(count, na.rm = TRUE)) %>%
  mutate(AUC_USG = AUC / USG) %>%
  mutate(AUC_CREAT = AUC / CREAT) %>%
  mutate(AUC_EGFR = AUC / EGFR)
}

# add_auc_slices <- function(df, from, to){
#   df %>%
#     group_by_at(vars(-size, -count)) %>%
#     filter(size >= from, size < to) %>%
#     mutate(auc_slice = sum(count, na.rm = TRUE)) %>%
#     ungroup() %>%
#     select(patient, auc_slice) %>%
#     distinct() %>%
#     set_names(c("patient", str_c("slice_", str_pad(from, 3, "left", "0"),
#                                  "_",
#                                  str_pad(to, 3, "left", 0))))
# }







#
# record…
# 0-150 sub AUC (small) / 150 - 500 sub AUC (large)
#
# record...
# sub AUC 20-50, 50 - 80 etc
#
#
# for each of these record them /UrCR, /USG, /AUC and /(small / large AUC)
#
#
#
# (no point dividing as a ratio!)
#
# then join all these to coin path data, label by filename, case, day, species
