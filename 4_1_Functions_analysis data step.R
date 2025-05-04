##Neutral functions
identity <- function(step, param){
  return(1)
}

##Everything related to "efficiency": how much of acceleration is used in which direction
fw_relative <- function(step, param){
  return(sum(abs(step$acc_fw)) / (sum(abs(step$acc_fw)) + sum(abs(step$acc_vert)) + sum(abs(step$acc_side))))
}

side_relative <- function(step, param){
  return(sum(abs(step$acc_side)) / (sum(abs(step$acc_fw)) + sum(abs(step$acc_vert)) + sum(abs(step$acc_side))))
}

vert_relative <- function(step, param){
  return(sum(abs(step$acc_vert)) / (sum(abs(step$acc_fw)) + sum(abs(step$acc_vert)) + sum(abs(step$acc_side))))
}

##Everything about "size" acceleration: how much one accelerates when walking

fw_absolute <- function(step, param){
  return(sum(abs(step$acc_fw)))
}

side_absolute <- function(step, param){
  return(sum(abs(step$acc_side)))
}

vert_absolute <- function(step, param){
  return(sum(abs(step$acc_vert)))
}

#Because actually only negative acceleration takes energy. Positive acceleration is from gravity
vert_upwards_absolute <- function(step, param){
  vert_pos_idc <- which(step$acc_vert < 0)
  -sum(step$acc_vert[vert_pos_idc])
}

fw_max <- function(step, param){
  return(max(abs(step$acc_fw)))
} #Risk is that shorter frequencies yield higher values. Still maybe interesting

vert_max <- function(step, param){
  return(max(abs(step$acc_vert)))
}

side_max <- function(step, param){
  return(max(abs(step$acc_side)))
}

##Peak vs valley
#See how high acceleration is at most extreme point when accelerating up compared to down. Inverted already multiplied by -1 right?!
val_v_peak <- function(step, param){
  abs(min(step$acc_vert))/abs(max(step$acc_vert))
}

##"Explosiveness" (vert?) / Jerkiness (side/fw?): how concentrated acceleration is
#Has to be normalised: in very short it has to be concentrated in a few
explosive_fw <- function(step, param){
  return(max(abs(step$acc_fw))/sum(abs(step$acc_fw))* nrow(step))
}

explosive_vert <- function(step, param){
  return(max(abs(step$acc_vert))/sum(abs(step$acc_vert)) * nrow(step))
}

explosive_side <- function(step, param){
  return(max(abs(step$acc_side))/sum(abs(step$acc_side))* nrow(step))
}

##Everything about regularity
##Variance
fw_variance <- function(step, param){
  return(var(step$acc_fw))
}

side_variance <- function(step, param){
  return(var(step$acc_side))
}

vert_variance <- function(step, param){
  return(var(step$acc_vert))
}

##Small changes / sign changes in differences compared to total number of differences
sign_changes_fw <- function(step, param){
  return(sum(diff(sign(diff(step$acc_fw))) != 0, na.rm = T)/(nrow(step) - 2))
  #Note that nrow(step) - 2 is the maximum possible number of sign change sequences.
  #E.g., c(1, 3, 2, 5, 6, 2, 1, 6) has four changes out of 6 possible, but 8 entries.
}

sign_changes_vert <- function(step, param){
  return(sum(diff(sign(diff(step$acc_vert))) != 0, na.rm = T)/(nrow(step) - 2))
}

sign_changes_side <- function(step, param){
  return(sum(diff(sign(diff(step$acc_side))) != 0, na.rm = T)/(nrow(step) - 2))
}

##Correlation with itself at a certain lag
cor_fw_cyclic_normalised <- function(step, param) {
  n <- nrow(step)
  lag <- round(param[1] * nrow(step)) #Here the parameter is a value between 0 and 1
  vec1 <- step$acc_fw
  vec2 <- step$acc_fw
  
  vec2_shifted <- c(tail(vec2, -lag), head(vec2, lag))
  if(lag > 0){
    return(cor(vec1, vec2_shifted))
  }
  else{
    return(cor(vec1, vec2))
  }
}

cor_vert_cyclic_normalised <- function(step, param) {
  n <- nrow(step)
  lag <- round(param[1] * nrow(step)) #Here the parameter is a value between 0 and 1
  vec1 <- step$acc_vert
  vec2 <- step$acc_vert
  
  vec2_shifted <- c(tail(vec2, -lag), head(vec2, lag))
  if(lag > 0){
    return(cor(vec1, vec2_shifted))
  }
  else{
    return(cor(vec1, vec2))
  }
}

cor_side_cyclic_normalised <- function(step, param) {
  n <- nrow(step)
  lag <- round(param[1] * nrow(step)) #Here the parameter is a value between 0 and 1
  vec1 <- step$acc_side
  vec2 <- step$acc_side
  
  vec2_shifted <- c(tail(vec2, -lag), head(vec2, lag))
  if(lag > 0){
    return(cor(vec1, vec2_shifted))
  }
  else{
    return(cor(vec1, vec2))
  }
}

#Multivariate: taking into account relation between the acceleration values
##Correlation, because invariant to changes
#Can be taken at any lag, but when we want to compare, should probably consider that
#a step of length 30 at lag 3 is comparable with lag 2 for step of length 20
cor_fw_side_cyclic <- function(step, param) {
  n <- nrow(step)
  lag <- param[1] #Assume within right range
  vec1 <- step$acc_fw
  vec2 <- step$acc_side
  
  vec2_shifted <- c(tail(vec2, -lag), head(vec2, lag))
  if(lag > 0){
    return(cor(vec1, vec2_shifted))
  }
  else{
    return(cor(vec1, vec2))
  }
}

cor_fw_vert_cyclic <- function(step, param) {
  n <- nrow(step)
  lag <- param[1] #Assume within right range
  vec1 <- step$acc_fw
  vec2 <- step$acc_vert
  
  vec2_shifted <- c(tail(vec2, -lag), head(vec2, lag))
  if(lag > 0){
    return(cor(vec1, vec2_shifted))
  }
  else{
    return(cor(vec1, vec2))
  }
}

cor_vert_side_cyclic <- function(step, param) {
  n <- nrow(step)
  lag <- param[1] #Assume within right range
  vec1 <- step$acc_vert
  vec2 <- step$acc_side
  
  vec2_shifted <- c(tail(vec2, -lag), head(vec2, lag))
  if(lag > 0){
    return(cor(vec1, vec2_shifted))
  }
  else{
    return(cor(vec1, vec2))
  }
}

cor_fw_side_cyclic_normalised <- function(step, param) {
  n <- nrow(step)
  lag <- round(param[1] * nrow(step)) #Here the parameter is a value between 0 and 1
  vec1 <- step$acc_fw
  vec2 <- step$acc_side
  
  vec2_shifted <- c(tail(vec2, -lag), head(vec2, lag))
  if(lag > 0){
    return(cor(vec1, vec2_shifted))
  }
  else{
    return(cor(vec1, vec2))
  }
}

cor_fw_vert_cyclic_normalised <- function(step, param) {
  n <- nrow(step)
  lag <- round(param[1] * nrow(step)) #Here the parameter is a value between 0 and 1
  vec1 <- step$acc_fw
  vec2 <- step$acc_vert
  
  vec2_shifted <- c(tail(vec2, -lag), head(vec2, lag))
  if(lag > 0){
    return(cor(vec1, vec2_shifted))
  }
  else{
    return(cor(vec1, vec2))
  }
}

cor_vert_side_cyclic_normalised <- function(step, param) {
  n <- nrow(step)
  lag <- round(param[1] * nrow(step)) #Here the parameter is a value between 0 and 1
  vec1 <- step$acc_vert
  vec2 <- step$acc_side
  
  vec2_shifted <- c(tail(vec2, -lag), head(vec2, lag))
  if(lag > 0){
    return(cor(vec1, vec2_shifted))
  }
  else{
    return(cor(vec1, vec2))
  }
}

# ! Add functions that output at which fraction we find the maximum correlation, both within one axis and between axes
