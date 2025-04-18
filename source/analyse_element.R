#roxygen2 header
#' @title analyse_element
#' @description Analyse the data of a specific element
#' @param data A tibble with the cleaned data
#' @param chem_element The element to analyse, e.g. 'ICP_Al'
#' @return A list with the data, a summary of the data, the model summary and a plot of the residuals
#' @export
#' @importFrom lme4 lmer
analyse_element <-  function(data, chem_element = NULL) {
  #check input variables
  if (is.null(chem_element)) {
    chem_element <- unique(data$element)
    if (length(chem_element) > 1) {
      stop(paste0("multiple elements found: ",
                  paste(chem_element, collapse = ", ")))
    }
  }

  #get element specific data (project data, no below loq data)
  data_el0 <- data |>
    filter(element == chem_element,
           type == "project")

  data_el <- data_el0 |>
    filter(!is.na(filter),
           !is.na(value),
           !all_below_loq)

  #summarise data
  data_summary <-
    data.frame(field = c("total project measurements",
                         "measurements after NA and LOQ filter",
                         "unique samples"),
               value = c(nrow(data_el0),
                          nrow(data_el),
                         length(unique(data_el$lab_id))))

  # create model

  if (length(unique(data_el$matrix))> 1) {
    mod_formula <- log10_value ~ 0 + matrix * filter + (1|lab_id)
  } else {
    mod_formula <- log10_value ~ filter + (1|lab_id)
  }
  if (data_el$n_filters[1] > 0) {
    mod_formula <- update(mod_formula, . ~ . + n_filters)
  }
  mod <- lmer(mod_formula, data = data_el)
  mod_summary <- summary(mod)

  p_fit_res <-
    ggplot(data.frame(resid = resid(mod, type = "pe"), fit = fitted(mod)),
         aes(x = fit, y = resid)) +
    geom_abline(intercept = 0, slope = 0, linetype = "dashed") +
    geom_smooth(method = "loess",formula = y ~ x) +
    geom_point() +
    labs(title = "Pearson residuals plot")

  data_el$fitted <- NA
  data_el$fitted[!is.na(data_el$n_filters)] <- fitted(mod)

  p_res_hist <- ggplot(data = data.frame(x = resid(mod, type = "pe")),
                       aes(x = x)) + geom_histogram(bins = 30)

  p_res_qq <- ggplot(data = data.frame(x = resid(mod, type = "pe")),
                     aes(sample = x)) +
    geom_qq() + geom_qq_line()

  plot_resid <- (p_fit_res | (p_res_hist / p_res_qq))

  ### prediction plot
  newdata <-
    expand.grid(
      log10_value =
        seq(min(data_el$log10_value, na.rm = TRUE),
            max(data_el$log10_value, na.rm = TRUE),
            length = 10),
      filter = na.omit(unique(data_el$filter)),
      matrix = unique(data_el$matrix),
      n_filters = range(data_el$n_filters, na.rm = TRUE),
      lab_id = 0)

  plotdata <-
    bind_cols(
      newdata,
      predictInterval(mod,
                      newdata = newdata,
                      level = 0.95,
                      which = "fixed",
                      include.resid.var = FALSE))
  plotdata_pivot <- plotdata |>
    pivot_wider(names_from = filter, values_from = c(fit, lwr, upr))

  ggplot(plotdata, aes(x = fit, y = log10_value, color = filter)) +
    geom_point() +
    geom_errorbar(aes(ymin = lwr, ymax = upr)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    facet_grid(matrix ~ n_filters) +
    labs(title = "Prediction plot")



  list(data = data_el,
       data_summary = data_summary,
       model = mod_summary,
       plot_resid = plot_resid,
       plot_model = plot_model)
}



#' @title plot_element
#' @description Plot the log10 values of an element in two different filters
#' @param data A tibble with the cleaned data
#' @param chem_element The element to plot, e.g. 'ICP_Al'
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#'
analyse_element_old <- function(data, chem_element = NULL){

  print(chem_element)
  if (is.null(chem_element)) {
    chem_element <- unique(data$element)
    if (length(chem_element) > 1) {
      stop(paste0("multiple elements found: ",
                  paste(chem_element, collapse = ", ")))
    }
  }

  data_el <- get_proj_elementdata(data, chem_element)
  print(dim(data_el))

  p_data <-
    ggplot(data_el, aes(x = f_20_micron, y = f_45_micron)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    geom_smooth(method = "lm") +
    geom_point() +
    labs(title = paste0("log10-log10 relation of ", chem_element))

  if (length(unique(data_el$matrix))> 1) {
    mod <- lm(diff_log10 ~ 0 + matrix * f_20_micron, data = data_el)
  } else {
    mod <- lm(diff_log10 ~ f_20_micron, data = data_el)
  }
  if (data_el$n_filters[1] > 0) {
    mod <- update(mod, . ~ . + n_filters)
  }

  mod_summary <- summary(mod)

  p_model <-
    ggplot(data.frame(resid = resid(mod), fit = fitted(mod)),
         aes(x = fit, y = resid)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    geom_smooth() +
    geom_point() +
    labs(title = "Residuals plot")
  list(data = data_el, model = mod_summary, plot_model = p_model)
}
