##Neutral
seq_length <- function(seq, param){
  return(nrow(seq$data))
}

# ##Everything related to regularity: are steps alike
# variance_length_middle <- function(seq, param){
# #Returns a single value
#   
#   normalised <- param[1]
#   
#   step_lengths <- c()
#   
#   end_bw <- max(seq$data$in_step_bw)
#   end_fw <- max(seq$data$in_step_fw)
#   
#   if(end_bw > 1){
#     for(j in 1:(end_bw - 1)){
#       step <- filter(seq$data, in_step_bw == j)
#       step_lengths <- append(step_lengths, nrow(step))
#     }
#   }
#   else{
#     step_lengths <- append(step_lengths, nrow(filter(seq$data), in_step_bw == 1))
#   }
#   
#   if(end_fw > 1){
#     for(j in 1:(end_fw - 1)){
#       step <- filter(seq$data, in_step_fw == j)
#       step_lengths <- append(step_lengths, nrow(step))
#     }
#   }
#   else{
#     step_lengths <- append(step_lengths, nrow(filter(seq$data), in_step_fw == 1))
#   }
#   
#   if(normalised == F){
#     return(var(step_lengths))
#   }
#   else{
#     return(var(step_lengths)/(mean(step_lengths)))
#   }
# }

variance_length_all <- function(seq, param){
#Returns a single value  
  
  normalised <- param[1]
  
  step_lengths <- c()
  
  end_bw <- max(seq$data$in_step_bw)
  end_fw <- max(seq$data$in_step_fw)
  
  for(j in 1:end_bw){
    step <- filter(seq$data, in_step_bw == j)
    step_lengths <- append(step_lengths, nrow(step))
  }
  
  for(j in 1:end_fw){
    step <- filter(seq$data, in_step_fw == j)
    step_lengths <- append(step_lengths, nrow(step))
  }
  
  if(normalised == F){
    return(var(step_lengths))
  }
  else{
    return(var(step_lengths)/(mean(step_lengths)))
  }
}

# differences_length_middle <- function(seq, param){
#   #Returns a vector: for every single step how much different from the mean number
#   
#   step_diffs <- c()
#   
#   end_bw <- max(seq$data$in_step_bw)
#   end_fw <- max(seq$data$in_step_fw)
#   mean_len <- nrow(seq$data)/(end_bw + end_fw)
#   
#   if(end_bw > 1){
#     for(j in 1:(end_bw-1)){
#       step <- filter(seq$data, in_step_bw == j)
#       step_diffs <- append(step_diffs, mean_len - nrow(step))
#     }
#   }
#   else{
#     step_diffs <- append(step_diffs, mean_len - nrow(filter(seq$data), in_step_bw == 1))
#   }
#   
#   if(end_fw > 1){
#     for(j in 1:(end_fw-1)){
#       step <- filter(seq$data, in_step_fw == j)
#       step_diffs <- append(step_diffs, mean_len - nrow(step))
#     }
#   }
#   else{
#     step_diffs <- append(step_diffs, mean_len - nrow(filter(seq$data), in_step_fw == 1))
#   }
#   
#   return(step_diffs) 
# }

differences_length_all <- function(seq, param){
  #Returns a vector: for every single step how much different from the mean number
  
  step_diffs <- c()
  
  end_bw <- max(seq$data$in_step_bw)
  end_fw <- max(seq$data$in_step_fw)
  mean_len <- nrow(seq$data)/(end_bw + end_fw)
  
  for(j in 1:end_bw){
    step <- filter(seq$data, in_step_bw == j)
    step_diffs <- append(step_diffs, mean_len - nrow(step))
  }
  
  for(j in 1:end_fw){
    step <- filter(seq$data, in_step_fw == j)
    step_diffs <- append(step_diffs, mean_len - nrow(step))
  }
  if(param[1] == T){
    return(step_diffs / mean_len)
  }
  else{
    return(step_diffs) 
  }
}

correlations_at_lag_mean <- function(seq, param){
  no_fw <- length(seq$cor_fw)
  if(no_fw > 1){
    correlations <- append(seq$cor_bw, seq$cor_fw[2:no_fw])
  }
  else{
    correlations <- seq$cor_bw
  }
  return(mean(correlations))
}

correlations_at_lag_var <- function(seq, param){
  # correlations <- c()
  # no_fw <- length(seq$cor_fw)
  # if(no_fw > 1){
  #   correlations <- append(seq$cor_bw, seq$cor_fw[2:no_fw])
  # }
  # else{
  #   correlations <- seq$cor_bw
  # }
  
  #Unavoidable for now due to NA otherwise. 
  #Sequences of length 2 should just not be omitted, filtered out when using this data seq metric, and then use old
  correlations <- append(seq$cor_bw, seq$cor_fw) 
  return(var(correlations))
}

kth_hill_fft_fw <- function(seq, param){
  
  ma_factor <- param[1] #Around 60 makes sense?
  k <- param[2]
  
  ma_len <- floor(seq$len/ma_factor) 
  
  if (ma_len %% 2 == 0) {
    ma_param <- ma_len + 1
  } else {
    ma_param <- ma_len  
  }
  
  seq_spec <- energy_kernel(seq$data$acc_fw, ma_param, ma_gauss, sigma = 1)
  k_hill_prop <- capture_hills(seq_spec, k)[[3]][[k]]
  return(k_hill_prop)
}

kth_hill_fft_vert <- function(seq, param){
  
  ma_factor <- param[1]
  k <- param[2]
  
  ma_len <- floor(seq$len/ma_factor)
  
  if (ma_len %% 2 == 0) {
    ma_param <- ma_len + 1
  } else {
    ma_param <- ma_len  
  }
  
  seq_spec <- energy_kernel(seq$data$acc_vert, ma_param, ma_gauss, sigma = 1)
  k_hill_prop <- capture_hills(seq_spec, k)[[3]][[k]]
  return(k_hill_prop)
}

kth_hill_fft_side <- function(seq, param){
  
  ma_factor <- param[1]
  k <- param[2]
  
  ma_len <- floor(seq$len/ma_factor)
  
  if (ma_len %% 2 == 0) {
    ma_param <- ma_len + 1
  } else {
    ma_param <- ma_len  
  }
  
  seq_spec <- energy_kernel(seq$data$acc_side, ma_param, ma_gauss, sigma = 1)
  k_hill_prop <- capture_hills(seq_spec, k)[[3]][[k]]
  return(k_hill_prop)
}

## Everything related to phases within step


##Everything related to starting/stopping issues, fatigue etc. -> e.g. movement start vs end


##Everything related to relationship of movements within overarching one 

