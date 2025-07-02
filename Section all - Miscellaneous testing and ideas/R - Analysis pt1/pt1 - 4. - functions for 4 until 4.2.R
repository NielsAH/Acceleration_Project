block <- function(t,n){
  value <- 0
  
  for(j in 1:n){
    value <- value + (1/n)*cos((2*pi*(n-1)*t)/(n))
  }
  
  return(value)
}


inverse_fourier <- function(spectrum, t){
  
  N <- length(spectrum)
  real <- Re(spectrum)
  imag <- Im(spectrum)
  
  value <- 0
    
  for(k in 0:(N-1)){
    value <- value + real[k+1]*cos(2*pi*k*(t-1)/N) - imag[k+1]*sin(2*pi*k*(t-1)/N)
  }  

  return(value/N)
}

inverse_fourier_approx <- function(spectrum, t, num_comp){
  
  N <- length(spectrum)
  
  indices <- (Mod(spectrum) %>% order(decreasing = T))[1:num_comp]
  approx_spectrum <- numeric(N)
  approx_spectrum[indices] <- spectrum[indices]
  
  real <- Re(approx_spectrum)
  imag <- Im(approx_spectrum)
  
  value <- 0
  
  for(k in 0:(N-1)){
    value <- value + real[k+1]*cos(2*pi*k*(t-1)/N) - imag[k+1]*sin(2*pi*k*(t-1)/N)
  }  
  
  return(value/N)
}
