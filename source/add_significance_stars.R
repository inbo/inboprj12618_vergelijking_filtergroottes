#' Add significance stars to summary coefficients
#'
#' @param smrycoef A summary of the coefficients from a linear model
#'
#' @returns A tibble with the coefficients and significance stars
#' @export
#'
add_significance_stars <- function(smrycoef) {
  smrycoef <- smrycoef |> as.data.frame()
  if (!("Pr(>|t|)" %in% colnames(smrycoef))) {
    smrycoef <- smrycoef |>
      mutate(`Pr(>|t|)` = 2 * pnorm(-abs(`t value`)))
  }
  smrycoef <- smrycoef |>
    mutate(sig = cut(`Pr(>|t|)`,
                     breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                     labels = c("***", "**", "*", ".", "")))
  smrycoef
}
