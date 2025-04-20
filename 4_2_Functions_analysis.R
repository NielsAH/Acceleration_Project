colQuantile <- function(df, frac) {
  apply(df, 2, quantile, probs = frac)
}