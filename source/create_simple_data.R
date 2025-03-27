
#' Create simple data
#'
#' @param data A tibble with the cleaned data
#'
#' @returns A list with long and wide format data
#' @export
create_simple_data <- function(data) {
  # Create the dataset with only "grondwater" as matrix and "project" as type
  simple_data <- data %>%
    filter(matrix == "grondwater",
           type == "project",
           !all_below_loq) |>
    mutate(value_orig = value,
           value = ifelse(value_orig < 0, 0.0001, value_orig),
           log10_value = log10(value)) |>
    select(lab_id, element, value, log10_value,
           filter, matrix, is_below_loq, all_below_loq)

  # Create the wide format data
  simple_data_wide <- simple_data |>
    pivot_wider(names_from = filter, values_from = c(value, log10_value))

  # Create the list with the long and wide format data
  data_list_l <- simple_data |>
    group_by(element) |>
    summarise(
      long = list(pick(everything())))

  data_list_w <- simple_data_wide |>
    mutate(mean_log10_value = 1/2 * (log10_value_f_20_micron + log10_value_f_45_micron),
           diff_log10_value = log10_value_f_20_micron - log10_value_f_45_micron) |>
    group_by(element) |>
    summarise(
      wide = list(pick(everything()))
    )

  # put the two lists together
  inner_join(data_list_l, data_list_w, by = "element")
}
