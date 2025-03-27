
#' Title
#'
#' @param dataw filtered only projectsamples wide data
#' @param chem_element
#'
#' @return
#' @export
#'
#' @examples
analyse_diff_element <-  function(dataw, chem_element) {
  #check input variables
  if (is.null(chem_element)) {
    chem_element <- unique(data$element)
    if (length(chem_element) > 1) {
      stop(paste0("multiple elements found: ",
                  paste(chem_element, collapse = ", ")))
    }
  }

  #get element specific data
  data_el <- dataw |>
    filter(element == chem_element)

  #get model formula
  mod_fmla_full <- diff_log10_value ~
    log10_value_f_20_micron + matrix + diff_n_filters +
    log10_value_f_20_micron:diff_n_filters

  mod_f <- lm(mod_fmla_full, data = data_el)


}
