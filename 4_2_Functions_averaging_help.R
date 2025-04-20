#Possible start functions comparing average step within movement with total average so far
min_square_difference_vert <- function(list_fw_vert_side_afw_avert_aside){
  avg_mvm <- list_fw_vert_side_afw_avert_aside[[2]]
  avg_sofar <- list_fw_vert_side_afw_avert_aside[[5]]
  
  m <- length(avg_sofar)
  
  diff_list <- numeric(m)
  
  k <- 1
  diff_list[1] <- norm(avg_mvm - avg_sofar, type = '2')
  
  for(k in 2:m){
    shifted_avg_mvm <-  c(avg_mvm[k:m], avg_mvm[1:(k-1)])
    diff_list[k] <- norm(shifted_avg_mvm - avg_sofar, type = '2')
  }
  
  return(which.min(diff_list))
}

#Possible start functions based purely on average steps within the movement
default_avg <- function(list_fw_vert_side){
  return(1) #As in: no shift
}

max_norm <- function(list_fw_vert_side){
  which.max(list_fw_vert_side[[1]]^2 + list_fw_vert_side[[2]]^2 + list_fw_vert_side[[3]]^2)
}

min_fw <- function(list_fw_vert_side){
  return(which.min(list_fw_vert_side[[1]]))
}

max_fw <- function(list_fw_vert_side){
  return(which.max(list_fw_vert_side[[1]]))
}

min_vert <- function(list_fw_vert_side){
  return(which.min(list_fw_vert_side[[2]]))
}

max_vert <- function(list_fw_vert_side){
  return(which.max(list_fw_vert_side[[2]]))
}

