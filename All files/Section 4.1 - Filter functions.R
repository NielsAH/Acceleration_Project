#Filter function
default <- function(seq, param){
  return(T)
}

freq_selector <- function(seq, param){
  min <- param[1]
  max <- param[2]
  if(seq$main_freq > min & seq$main_freq < max){
    return(T)
  }
  else{
    return(F)
  }
}
