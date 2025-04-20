#1
quantile_bottom <- function(vector, fraction){
  return(quantile(vector, fraction)[[1]])
}

#2
mean_alt <- function(vector, unused){
  return(mean(vector))
}

#3
trimmed_mean <- function(vector, trim_frac) {
  lower <- quantile(vector, trim_frac)[[1]]
  upper <- quantile(vector, 1 - trim_frac)[[1]]
  mean(vector[vector >= lower & vector <= upper])
}

#4
extreme_frac <- function(vector, metric_par){
  mult_fac_larger <- metric_par[1]
  mult_fac_smaller <- metric_par[2]
  #The fraction of observations that is an outlier, "far from the mean"
  (length(vector[vector > (mean(vector) * mult_fac_larger)]) + length(vector[vector < (mean(vector)*mult_fac_smaller)]))/length(vector)
}

#5
var_alt <- function(vector, unused){
  return(var(vector))
}