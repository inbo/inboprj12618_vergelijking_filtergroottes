#' @title Create diffs data
#'
#' @param data A tibble with the cleaned data
#'
#' @return A tibble with the differences in log10 values and number of filters
#' @importFrom dplyr filter select pivot_wider mutate
#' @export
#'
create_diffs_data <- function(data){
  dataw <- data |>
    filter(type == "project",
           !is.na(filter),
           !is.na(log10_value),
           !all_below_loq) |>
    select(lab_id, element, matrix, filter, log10_value, n_filters) |>
    pivot_wider(names_from = filter, values_from = c(log10_value, n_filters)) |>
    mutate(diff_log10_value = log10_value_f_20_micron - log10_value_f_45_micron,
           diff_n_filters = n_filters_f_20_micron - n_filters_f_45_micron)
  dataw
}
