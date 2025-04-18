analyse_element_simple_fmod <- function(data, chem_element) {
  require("tidyverse")
  require("patchwork")

  data_el <- data |> filter(element == chem_element) |> pull(long)

  log_name <- paste0("log10(", chem_element, ")")
  data_el <- data_el[[1]] |>
    filter(is.finite(log10_value),
           !is.na(filter),
           !is.na(value),
           !all_below_loq) |>
    mutate(element = chem_element,
           !!log_name := log10_value,
           filter2 = factor(filter,
                            levels = sort(unique(filter)),
                            labels = c("20µm", "45µm")))

  #mod_formula <- log10_value ~ filter + (1|lab_id)
  mod_formula <- as.formula(paste0("`", log_name, "` ~ filter + (1|lab_id)"))


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

  minrange <- min(data_el$log10_value, na.rm = TRUE)
  maxrange <- max(data_el$log10_value, na.rm = TRUE)

  # newdata_fmod <-
  #   expand.grid(
  #     log10_value =
  #       seq(from = minrange,
  #           to = maxrange,
  #           length = 20),
  #     filter = na.omit(unique(data_el$filter)),
  #     matrix = unique(data_el$matrix),
  #     lab_id = 0)

  # Create a named list for expand.grid
  grid_params <- list()
  grid_params[[log_name]] <- seq(from = minrange, to = maxrange, length = 20)
  grid_params[["filter"]] <- na.omit(unique(data_el$filter))
  grid_params[["matrix"]] <- unique(data_el$matrix)
  grid_params[["lab_id"]] <- 0

  # Use do.call to pass the list to expand.grid
  newdata_fmod <- do.call(expand.grid, grid_params)

  #fmod: value ~ filter

  mod <- lme4::lmer(mod_formula, data = data_el)
  mod_summary <- summary(mod)

  ### Residual plots fmod

  p_fit_res <-
    ggplot(data.frame(resid = resid(mod, type = "pe"), fit = fitted(mod)),
           aes(x = fit, y = resid)) +
    geom_abline(intercept = 0, slope = 0, linetype = "dashed") +
    geom_smooth(method = "loess",formula = y ~ x) +
    geom_point() +
    labs(x = "fitted values", y = "pearson residuals")

  p_res_hist <-
    ggplot(data = data.frame(x = resid(mod, type = "pe")),
           aes(x = x)) +
    geom_histogram(bins = 20) +
    labs(x = "pearson residuals", y = "count")

  p_res_qq <-
    ggplot(data = data.frame(x = resid(mod, type = "pe")),
           aes(sample = x)) +
    geom_qq() +
    geom_qq_line() +
    labs(x = "theoretical quantiles", y = "sample quantiles")

  plot_resid <- (p_fit_res | (p_res_hist / p_res_qq))

  ###
  # residual criteria
  data_conclusions <- nobs_discussion(data_el, factor = 2)
  norm_conclusions <- normality_discussion(resid(mod, type = "pe"))

  ### Prediction plot fmod

  plotdata_fmod <-
    bind_cols(
      newdata_fmod,
      predictInterval(mod,
                      newdata = newdata_fmod,
                      level = 0.95,
                      which = "fixed",
                      include.resid.var = FALSE,
                      fix.intercept.variance = TRUE)) |>
    mutate(filter = factor(filter,
                           levels = sort(unique(data_el$filter)),
                           labels = c("20µm", "45µm")))

  plotdata_fmod_pivot <- plotdata_fmod |>
    pivot_wider(names_from = filter, values_from = c(fit, lwr, upr))

  plot_model <-
    ggplot() +
    geom_jitter(data = data_el, aes(x = filter2, y = log10_value, color = filter2), width = 0.05) +
    geom_line(data = data_el, aes(x = filter2, y = log10_value, group = lab_id),
              color = "black", alpha = 0.2) +
    geom_point(plotdata_fmod, mapping = aes(y = fit, x = filter), color = "black") +
    geom_errorbar(plotdata_fmod, mapping = aes(x = filter, ymin = lwr, ymax = upr)) +
    labs(x = "filter", y = paste0("log10(", chem_element, ")"), color = "filter")

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
    geom_errorbar(width = 0.75) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "blue", linewidth = 1.3) +
    xlim(0.5,1.5) +
    labs(y = "log10(45µm) - log10(20µm)", x = paste("element:",chem_element)) +
    theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())

  plot_pred <- plot_model | (plot_model_ref / plot_model_fit_only)

  ### prediction conclusion
  model_conclusions <- trend_fmod_discussion(mod_summary)

  list(data = data_el,
       formula = mod_formula,
       model = mod_summary,
       plot_resid = plot_resid,
       plot_pred = plot_pred,
       model_conclusions = list(data = data_conclusions,
                                res = norm_conclusions,
                                model = model_conclusions))

}
