check_cleaned_data <- function(data) {
  ggplot(data, aes(x = log10(value))) +
    facet_wrap(~element, scales = "free") +
    geom_histogram()
}
