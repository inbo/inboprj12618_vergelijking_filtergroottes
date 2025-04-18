# Get element data with specific filters and types
#'
#' This function filters a dataset for specified chemical element and specific conditions.
#' It then selects relevant columns, reshapes the data, and creates a new column with the difference
#' between two log10 values.
#'
#' @param data A data frame containing the dataset.
#' @param chem_element A string representing the chemical element to filter by.
#'
#' @return A data frame with the specified columns, reshaped and containing a difference column.
#' @export
#'
#' @examples
#' df <- data.frame(
#'  lab_id = 1:4,
#'  element c("A", "B", "A", "B"),
#'  log10_value = run(4),
#'  filter =("f_20_micron, "f_45_micron", "f_20_micron", "f_45_micron"),
#'  n_filters = sample(1:5, 4, replace = TRUE),
#'  type = c("project", "project", "project", "other"),
#'  matrix = c("matrix1", "matrix2", "matrix1", "matrix2")
#'  )
#'  get_proj_elementdata(df, "A")
#'  get_proj_elementdata(df, "B")
get_proj_elementdata <- function(data, chem_element) {

  #aanpassen als er meer dan 2 zijn, de eerste 2 samen nemen en dan de volgende 2 enz
  data_c <- data |>
    filter(element == chem_element,
           !is.na(filter),
           type == "project") |> # remove rows where filter is NA
    arrange(element, matrix, lab_id, filter) |>
    group_by(element, matrix, lab_id, filter) |>
    mutate(sequence = row_number(),
           lab_id = paste0(lab_id, "_", sequence))

  filterdata <- data_c |>
    group_by(lab_id, element, matrix) |>
    summarise(n_meas = n(),
              n_filters_45 = mean(n_filters[filter == "f_45_micron"]),
              n_filters_20 = mean(n_filters[filter == "f_20_micron"]),
              diff_filters2045 = n_filters_20 - n_filters_45,
              .groups = "drop")

  valuedata  <- data_c |>
    group_by(lab_id, element, matrix) |>
    filter(element == chem_element,
           filter %in% c("f_20_micron", "f_45_micron"),
           type == "project",
           !all_below_loq) |> # remove rows where all values are below loq
    select(lab_id, element, log10_value, filter, matrix) |>
    pivot_wider(names_from = filter,
                values_from = log10_value,
                values_fn = function(x) mean(x, na.rm = TRUE)) |>
    mutate(diff_log10 = f_45_micron - f_20_micron)

  finaldata <- valuedata |>
    left_join(filterdata, by = c("lab_id", "element", "matrix"))

  finaldata
}

##############################################################################

