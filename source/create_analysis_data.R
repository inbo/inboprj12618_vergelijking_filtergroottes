
#' Title: Clean data
#'
#' @param imported A list with three dataframes: icp, ic and lod
#'
#' @return A tibble with the cleaned data
#' @export
#'
create_analysis_data <- function(imported) {
  data <- bind_rows(imported$icp, imported$ic) |>
    transmute(lab_id = sub("[ab]$", "", Labo_ID),
              type = ifelse(substring(lab_id,1,2) %in% c("BL") |
                              substring(lab_id, 1, 3) %in% c( "PBL", "SBL"),
                            "blank",
                            ifelse(substring(lab_id,1,1) == "2",
                                   "project",
                                   "reference")),
              filter = Filter,
              n_filters = `Aantal Filters Labo`,
              field_id = Veldcode,
              sampler = Staalnemer,
              matrix = tolower(Matrix),
              instrument,
              row_nr,
              element,
              value,
              log10_valuep25 = log10(value+0.25), #min -0.239
              #1747 op 15088 negatieve waarden worden NA
              #slechts 6 waar 1 van beide waarden boven de rapportagegrenzen liggen
              log10_value = log10(value)) |>
    filter(!is.na(value),
           !is.na(filter)) |>
    left_join(imported$loq |>
                dplyr::select(instrument, element, loq),
              join_by(instrument, element)) |>
    mutate(element = paste(instrument, element, sep = "_"),
           log10_loq = log10(loq),
           is_below_loq = log10_value < log10_loq) |>
    arrange(element, matrix, lab_id, filter) |>
    group_by(element, matrix, lab_id, filter) |>
    mutate(sequence = row_number()) |>
    ungroup() |>
    mutate(lab_id = ifelse(type == "project",
                           paste0(lab_id, "_", sequence),
                           lab_id)) |>
    group_by(element, lab_id) |>
    mutate(all_below_loq = sum(is_below_loq) == n()) |>
    group_by(element) |>
    mutate(filters_present = sum(!is.na(n_filters))) |>
    arrange(element, lab_id, filter)
  return(data)
}

