check_element_data <- function(data, chem_element) {
  dred <- data |> filter(element == chem_element)
  rv <- paste0("Total data: ", nrow(data), "\n",
               "Data for ", chem_element, ": ", nrow(dred))
  list(rv)
}
