analyse_element_simple_fmod <- function(data, chem_element) {
  require("tidyverse")
  require("patchwork")

  data_el <- data |> filter(element == chem_element) |> pull(long)
  data_el <- data_el[[1]] |>
    filter(is.finite(log10_value),
           !is.na(filter),
           !is.na(value),
           !all_below_loq)

  mod_formula <- log10_value ~ filter + (1|lab_id)

  #check if multiple lab_ids
  if (length(unique(data_el$lab_id)) < 3) {
    return(
      list(data = data_el,
           formula = mod_formula,
           model = NA,
           plot_resid = NA,
           plot_pred = NA
      )
    )
  }

  newdata_fmod <-
    expand.grid(
      log10_value =
        seq(from = min(data_el$log10_value, na.rm = TRUE),
            to = max(data_el$log10_value, na.rm = TRUE),
            length = 20),
      filter = na.omit(unique(data_el$filter)),
      matrix = unique(data_el$matrix),
      lab_id = 0)

  #fmod: value ~ filter

  mod_formula <- log10_value ~ filter + (1|lab_id)
  mod <- lme4::lmer(mod_formula, data = data_el)
  mod_summary <- summary(mod)

  ### Residual plots fmod

  p_fit_res <-
    ggplot(data.frame(resid = resid(mod, type = "pe"), fit = fitted(mod)),
           aes(x = fit, y = resid)) +
    geom_abline(intercept = 0, slope = 0, linetype = "dashed") +
    geom_smooth(method = "loess",formula = y ~ x) +
    geom_point() +
    labs(title = "Pearson residuals plot")

  p_res_hist <- ggplot(data = data.frame(x = resid(mod, type = "pe")),
                       aes(x = x)) + geom_histogram(bins = 30)

  p_res_qq <- ggplot(data = data.frame(x = resid(mod, type = "pe")),
                     aes(sample = x)) +
    geom_qq() + geom_qq_line()

  plot_resid <- (p_fit_res | (p_res_hist / p_res_qq))

  ### Prediction plot fmod

  plotdata_fmod <-
    bind_cols(
      newdata_fmod,
      predictInterval(mod,
                      newdata = newdata_fmod,
                      level = 0.95,
                      which = "fixed",
                      include.resid.var = FALSE,
                      fix.intercept.variance = TRUE))
  plotdata_fmod_pivot <- plotdata_fmod |>
    pivot_wider(names_from = filter, values_from = c(fit, lwr, upr))


  plot_model <-
    ggplot() +
    geom_jitter(data = data_el, aes(x = filter, y = log10_value, color = filter), width = 0.05) +
    geom_point(plotdata_fmod, mapping = aes(y = fit, x = filter), color = "black") +
    geom_errorbar(plotdata_fmod, mapping = aes(x = filter, ymin = lwr, ymax = upr)) +
    labs(title = "Prediction plot", xlab = "filter", ylab = paste0("log10(", chem_element, ")"))

  plot_model_fit_only <-
    ggplot(plotdata_fmod, aes(x = filter, y = fit, ymin = lwr, ymax = upr, color = filter)) +
    geom_point() +
    geom_errorbar() +
    labs(x = "filter", y = paste0("log10(", chem_element, ")"))

  plot_model_ref <-
    ggplot(data.frame(est = coef(mod_summary)[2,1],
                      lwr = coef(mod_summary)[2,1] + qnorm(0.025) * coef(mod_summary)[2,2],
                      upr = coef(mod_summary)[2,1] + qnorm(0.975) * coef(mod_summary)[2,2]),
           mapping = aes(x = 1, y = est, ymin = lwr, ymax = upr)) +
    geom_errorbar(width = 0.5) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "blue", linewidth = 2) +
    labs(y = "log10(45µm) - log10(20µm)", x = chem_element) +
    theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())

  plot_pred <- plot_model | (plot_model_ref / plot_model_fit_only)

  list(data = data_el,
       formula = mod_formula,
       model = mod_summary,
       plot_resid = plot_resid,
       plot_pred = plot_pred)

}
