#' Symmetry test
#'
#' @param resid A numeric vector of residuals from a linear model
#'
#' @returns A named vector with the difference between the mean and median of the residuals, and a string indicating the symmetry status
#' @export
#'
symmetry_test <- function(resid) {
  res_mean <- mean(resid)
  res_median <- median(resid)
  diff <- res_mean - res_median
  iqr <- IQR(resid)
  norm_mean_median_diff <- abs(diff) / iqr * 100
  # Determine symmetry status (AI proposal)
  symmetry_status <- case_when(
    norm_mean_median_diff < 5 ~ "symmetric",
    norm_mean_median_diff < 10 ~ "moderately symmetric",
    norm_mean_median_diff < 15 ~ "slightly skewed",
    TRUE ~ "skewed"
  )
  c(diff_criterion = norm_mean_median_diff,
    symmetry_status = symmetry_status)
}


normality_discussion <- function(x){
  norm_test <- shapiro.test(x)
  sym_test <- symmetry_test(x)
  shapiro_status <- case_when(
    norm_test$p.value < 0.001 ~ "De residu's wijken sterk af van de normalieit.",
    norm_test$p.value < 0.01 ~ "De residu's wijken af van de normaliteit",
    TRUE ~ "De residu's zijn voldoende normaal verdeeld."
  )
  sym_status <- case_when(
    sym_test[2] == "symmetric" ~ "De residu's zijn symmetrisch verdeeld.",
    sym_test[2] == "moderately symmetric" ~ "De residu's zijn vrij symmetrisch verdeeld.",
    sym_test[2] == "slightly skewed" ~ "De residu's zijn licht scheef verdeeld.",
    TRUE ~ "De residu's zijn scheef verdeeld."
  )
  paste(shapiro_status, sym_status, sep  = "\n")
}

trend_diff_discussion <- function(model_summary) {
  smry <- model_summary$coefficients
  intercept_p <- smry[1,4]
  slope_p <- smry[2,4]
  if (intercept_p < 0.001) {
    intercept_status <- "De intercept is sterk significant wat wijst op een verschil tussen de filters"
  } else if (intercept_p < 0.01) {
    intercept_status <- "De intercept is significant wat wijst op een verschil tussen de filters"
  } else if (intercept_p < 0.05) {
    intercept_status <- "De intercept is significant maar niet sterk significant wat kan wijzen op een verschil tussen de filters maar zonder sterk bewijs"
  } else {
    intercept_status <- "De intercept is niet significant. Dus er lijkt geen verschil tussen de filters te zijn."
  }
  if (slope_p < 0.001) {
    slope_status <- "De helling is sterk significant wat wijst dat het verschil tussen de filters afhangt van de concentratie die gemeten wordt."
  } else if (slope_p < 0.01) {
    slope_status <- " De helling is significant wat wijst dat het verschil tussen de filters afhangt van de concentratie die gemeten wordt."
  } else if (slope_p < 0.05) {
    slope_status <- "De helling is significant maar niet sterk significant wat kan wijzen dat het verschil tussen de filters afhangt van de concentratie die gemeten wordt maar zonder sterk bewijs."
  } else {
    slope_status <- " De helling is niet significant. Dus er lijkt geen verschil tussen de filters te zijn over het bereik van de gemeten concentratie."
  }
  paste(intercept_status, slope_status, sep = "\n")
}

trend_fmod_discussion <- function(model_summary) {
  smry <- model_summary$coefficients
  pval_slope <- 2 * pnorm(-abs(smry[2, "t value"]))
  if (pval_slope < 0.001) {
    slope_status <- "Volgens het mixed effect model is er een duidelijk significant verschil tussen de filters."
  } else if (pval_slope < 0.01) {
    slope_status <- "Volgens het mixed effect model is er een significant verschil tussen de filters van gepaarde metingen."
  } else if (pval_slope < 0.05) {
    slope_status <- "Volgens het mixed effect model is er een licht significant effect tussen de filters van gepaarde meingen zonder sterk bewijs."
  } else {
    slope_status <- "Met het mixed effect model kan er geen verschil tussen de filters van gepaarde metingen aangetoond worden."
  }
  slope_status
}

nobs_discussion <- function(data, factor = 1) {
  nobs <- nrow(data)
  if (nobs < factor * 10) {
    nobs_status <- "Het aantal observaties is laag, wat kan leiden tot onbetrouwbare conclusies"
  } else if (nobs < factor * 30) {
    nobs_status <- "Het aantal observaties is aan de lage kant, wat kan leiden tot enige onzekerheid in de resultaten."
  } else {
    nobs_status <- "Het aantal observaties is voldoende hoog, dus er is genoeg data om een verschil duidelijk te kunnen aantonen ."
  }
  nobs_status
}
