Big_window_manipulation <- function(df, wdw_size, ma_data, ma_energy, upright_thresh, abs_acc_thresh, kernel_fourier, ...){
  
  N <- nrow(df)
  
  #Defining empty vectors which we later add to the dataframe
  frequencies <- numeric(N)
  relative_frac <- numeric(N)
  absolute_frac <- numeric(N)
  frequencies_2 <- numeric(N)
  relative_frac_2 <- numeric(N)
  absolute_frac_2 <- numeric(N)
  
  upright_inverted <- numeric(N) #Whether device is upright and inverted (-1), upright not inverted (1), or not upright (0)
  
  #Define the new acceleration coordinates, after the first rotation (upsilon, xi*, zeta*)
  acc_vert <- numeric(N) 
  acc_side <- numeric(N) 
  acc_fw <- numeric(N)
  
  for(k in 1:N){
    if(k%%1500 == 0){
      print(paste("Progress: ", k/N))
    }
    
    start <- max(1, k - wdw_size)
    end <- min(N, k + wdw_size)
    window <- df[(start):(end),]
    
    rest_x <- mean(window$X)
    rest_y <- mean(window$Y)
    rest_z <- mean(window$Z)
    rest_vec <- c(rest_x, rest_y, rest_z)
    
    #Need to do something to invert? May be necessary?
    # if(rest_y > 0.5){ #Device worn wrong way: invert directions
    #   #df$Y[k] <- df$Y[k]* (-1) #Problem! Will mean that at next window this is already turned around
    #   #Really need approach per sequence
    #   #df$X[k] <- df$X[k] * (-1)
    #   rest_y <- rest_y * (-1)
    #   rest_x <- rest_x * (-1)
    # }
    # else if(rest_y < -0.5){
    #   #Do nothing, get correct acceleration
    # }
    # else{
    #   print(paste("Not clear how device is worn at index ", k))
    # }
    # #Could add if else // else: for example if not in right position
    
    rot_mat <- rotation_initial(c(rest_x, rest_y, rest_z))[[1]]
    #Also seems to work if rest_y > 0, thus device worn wrong way
    window[, 2:4] <- t(rot_mat %*% t(window[, 2:4]))
    
    
    if(mean(window[, 3]) > upright_thresh){
      upright_inverted[k] <- -1
    }
    else if(mean(window[,3] < -upright_thresh)){
      upright_inverted[k] <- 1
    }
    else{
      upright_inverted[k] <- 0
    }
    
    rot_Y <- t(rot_mat %*% t(df[k, 2:4]))[2] #y coordinate in that point
    rot_X <- t(rot_mat %*% t(df[k, 2:4]))[1] #horizontal one direction
    rot_Z <- t(rot_mat %*% t(df[k, 2:4]))[3] #horizontal orthogonal direction
    
    acc_vert[k] <- rot_Y
    acc_fw[k] <- rot_X
    acc_side[k] <- rot_Z
    
    #Note: window$Y is already rotated as well. Fully/correctly? Why no MA already here?
    spec <- window$Y %>% energy_kernel(., ma_energy, kernel_fourier, ...)
    
    hill_info <- capture_hills(spec, 2)
    frequencies[k] <- hill_info[[2]][[1]]
    relative_frac[k] <- hill_info[[3]][[1]]
    absolute_frac[k] <- hill_info[[4]][[1]]
    frequencies_2[k] <- hill_info[[2]][[2]]
    relative_frac_2[k] <- hill_info[[3]][[2]]
    absolute_frac_2[k] <- hill_info[[4]][[2]]
  }
  
  df$ma_Y <- ma_trunc(df$Y, n = ma_data)
  
  
  acc_vert <- acc_vert - mean(acc_vert) #Subtracting gravity 
  
  df$acc_vert <- acc_vert
  df$ma_vert <-  ma_trunc(df$acc_vert, n = ma_data)
  
  df$acc_side <- acc_side
  df$acc_fw <- acc_fw
  df$acc_hor <- sqrt(acc_side^2 + acc_fw^2)
  df$ma_hor <- ma_trunc(df$acc_hor, n = ma_data)
  
  df$row <- seq(1:N)
  df$frequencies <- frequencies
  df$relative_frac <- relative_frac
  df$absolute_frac <- absolute_frac
  df$frequencies_2 <- frequencies_2
  df$relative_frac_2 <- relative_frac_2
  df$absolute_frac_2 <- absolute_frac_2
  
  df$upright_inverted <- upright_inverted
  
  df$in_seq <- numeric(N) 
  df$in_step_fw <- numeric(N)
  df$in_step_bw <- numeric(N)
  
  df <- df %>%
    mutate(
      step_valley = ifelse(ma_vert < lag(ma_vert, default = Inf) & 
                             ma_vert < lead(ma_vert, default = Inf) & 
                             ma_vert < -abs_acc_thresh, 1, 0) #Changed: now just ''enough absolute acceleration'' (gravity helps, both if worn inverted and normal)
    )
  df <- df %>%
    mutate(
      step_peak = ifelse(ma_vert > lag(ma_vert, default = Inf) & 
                             ma_vert > lead(ma_vert, default = Inf) & 
                             ma_vert > abs_acc_thresh, 1, 0) #Changed: now just ''enough absolute acceleration'' (gravity helps, both if worn inverted and normal)
    )
  
  return(df)
}

Potential_sequence <- function(df, idx, thresh_abs, thresh_rel, min_step_period, max_step_period){
  
  if((df$absolute_frac[idx] < thresh_abs) | 
     (df$relative_frac[idx] < thresh_rel) |
     ((df$frequencies[idx]) < min_step_period) |
     ((df$frequencies[idx]) > max_step_period) |
     (df$in_seq[idx] != 0)
  ){
    return(F)
  }
  

    if(df$upright_inverted[idx] == -1){
      if(df$step_peak[idx] != 1){
        return(F)
      }
    }
    else if(df$upright_inverted[idx] == 1){
      if(df$step_valley[idx] != 1) {
        return(F)
      }
    }
    else{ #Thus when df$upright_inverted[idx] == 0
      return(F)
    }
  
  return(T)
}

Sequence_constructor <- function(seq_list, seq_num, df, idx, period_len, period_len_double, period_thresh, double_step_thresh){
  
  if(df$upright_inverted[idx] == -1){
    df$step_start <- df$step_peak
    inverted <- -1
  }
  else if(df$upright_inverted[idx] == 1){
    df$step_start <- df$step_valley
    inverted <- 1
  }
  else{
    print("Error has occurred. Starting index sequence is not upright even after filtering such sequences out.")
  }
  
  N <- nrow(df)
  
  freq <- round(df$frequencies[idx]*2) #For now we keep it simple. In reality may not want to multiply by 2, but fine to know we always do that for now
  rounding <- ifelse(((freq - floor(df$frequencies[idx]*2)) == 0), 1, -1)
  #Has value 1 if it was rounded down, e.g. 33.2 to 33. Has value -1 if it was rounded up
  
  if((idx + freq + 1 <= N) & (idx - freq - 1 >= 1)){
    if(sum(df$in_seq[(idx - freq - period_len_double) : (idx + freq + period_len_double)]) == 0 ) { #We need the values from the double step not yet in a sequence
      fw <- 0 #How far fw peak is. If stays 0: no peak
      bw <- 0 #How far bw peak is. If stays 0: no peak
      
      
      if(df$step_start[idx + freq] == 1){
        fw <- freq
      }
      else{ #If not at precise frequency a forward start (peak/valley)
        for(l in 1:period_len_double){
          if(df$step_start[idx + freq + rounding + (l - 1)] == 1){
            fw <- freq + rounding + (l-1)
          }
          else if(df$step_start[idx + freq - rounding - (l - 1)] == 1){
            fw <- freq - rounding - (l - 1)
          }
        }
      }
      
      if(df$step_start[idx - freq] == 1){
        bw <- freq
      }
      else{
        for(l in 1:period_len_double){
          if(df$step_start[idx - (freq + rounding + (l-1))] == 1){
            bw <- freq + rounding + (l - 1)
          }
          else if(df$step_start[idx - (freq - rounding - (l - 1))] == 1){
            bw <- freq - rounding - (l-1)
          }
        }
      }
      
      if((bw != 0) & (fw != 0)){ #We need a step forward and a step backward
        double_step <- df$ma_vert[(idx - bw : idx + fw)]
        acf_at_freq <- acf(double_step, lag.max = freq, plot = F)$acf[freq+1]
        if(acf_at_freq > double_step_thresh) 
        {
          seq_num <- seq_num + 1 #We really do have a new sequence, as we have at least two steps
          print(seq_num)
          seq_list[[seq_num]] <- list(data = data.frame(), cor_fw = c(), cor_bw = c(), main_freq = df$frequencies[idx], inverted = inverted, class = 1)
          
          step_num_fw <- 1
          step_num_bw <- 1 
        
          df$in_seq[(idx):(idx + fw - 1)] <- seq_num
          df$in_seq[c((idx - bw):(idx - 1))] <- seq_num
          df$in_step_fw[(idx):(idx + fw - 1)] <- step_num_fw
          df$in_step_bw[c((idx - bw):(idx - 1))] <- step_num_bw
          
          seq_list[[seq_num]]$cor_fw <- append(seq_list[[seq_num]]$cor_fw, acf_at_freq)
          seq_list[[seq_num]]$cor_bw <- append(seq_list[[seq_num]]$cor_bw, acf_at_freq)
          
          #From here go backward and forward to find more steps by checking if similar to last
          #First add steps to the sequence going forward
          
          forward <- T #Still chance of adding forward steps
          end_old <- idx
          end_new <- idx + fw #Current last one: we already know it is a valley
          
          while(forward){
            peak_found <- F
            
            if((end_new + freq + period_len) > N){
              
              
              forward <- F #We are at or close to end of dataframe. Cannot add new ones
              
            }
            else if(sum(df$in_seq[end_new:(end_new+freq+period_len)]) != 0){
              
              forward <- F 
              
              #We do not want to overlap with another sequence of movements. Later can add that we may want
              #to merge here // throw a message, but in principle indicates a break at some point,
              #as otherwise we would have detected this one earlier
            }
            else if(df$step_start[end_new + freq] == 1){
              #If enough periodicity: add step going forward
              
              acf_at_freq <- acf(df$ma_vert[end_old:(end_new + freq)], lag.max = freq, plot = F)$acf[freq + 1]
              
              if(acf_at_freq > period_thresh){
                step_num_fw <- step_num_fw + 1
                
                df$in_seq[(end_new):(end_new+freq - 1)] <- seq_num
                df$in_step_fw[(end_new):(end_new+freq - 1)] <- step_num_fw
                
                seq_list[[seq_num]]$cor_fw <- append(seq_list[[seq_num]]$cor_fw, acf_at_freq)
                
                peak_found <- T
                
                end_old <- end_new
                end_new <- end_new + freq
                
              } else{  #If little periodicity/correlation: stop collecting new steps 
                
                forward <- F
                
              }
            } else{ #If not at lag difference of 0, see if at any other lag
              for(k in 1:period_len){
                if (df$step_start[end_new + freq + k*rounding] == 1){
                  #If enough periodicity: add step going forward
                  #If little periodicity/correlation: stop collecting new steps
                  #If enough periodicity: add step going forward
                  
                  acf_at_freq <- acf(df$ma_vert[end_old:(end_new + freq + k*rounding)], lag.max = freq + k*abs(rounding), plot = F)$acf[floor((freq + freq + k*rounding)/2) + 1]
                  
                  if(acf_at_freq > period_thresh){
                    step_num_fw <- step_num_fw + 1
                    
                    df$in_seq[(end_new):(end_new+freq+k*rounding - 1)] <- seq_num
                    df$in_step_fw[(end_new):(end_new+freq+k*rounding - 1)] <- step_num_fw
                    
                    seq_list[[seq_num]]$cor_fw <- append(seq_list[[seq_num]]$cor_fw, acf_at_freq)
                    
                    peak_found <- T
                    
                    end_old <- end_new
                    end_new <- end_new + freq + k*rounding
                    
                  } else{  #If little periodicity/correlation: stop collecting new steps 
                    
                    forward <- F
                    
                  } 
                  
                  break
                }
                else if (df$step_start[end_new + freq - k*rounding] == 1){
                  #If enough periodicity: add step going forward
                  #If little periodicity/correlation: stop collecting new steps
                  
                  acf_at_freq <- acf(df$ma_vert[end_old:(end_new + freq - k*rounding)], lag.max = freq + k*abs(rounding), plot = F)$acf[floor((freq + freq - k*rounding)/2) + 1]
                  
                  if(acf_at_freq > period_thresh){
                    step_num_fw <- step_num_fw + 1

                    df$in_seq[(end_new):(end_new+freq-k*rounding - 1)] <- seq_num
                    df$in_step_fw[(end_new):(end_new+freq-k*rounding - 1)] <- step_num_fw
                    
                    seq_list[[seq_num]]$cor_fw <- append(seq_list[[seq_num]]$cor_fw, acf_at_freq)
                    
                    peak_found <- T
                    
                    end_old <- end_new
                    end_new <- end_new + freq - k*rounding
                  } else{  #If little periodicity/correlation: stop collecting new steps 
                    
                    forward <- F
                    
                  }   
                  
                  break
                }
                
              }
              
              #Here handle case that after this for-loop still no peak found: also
              #stop collecting new steps
              if(peak_found == F){
                forward <- F
              }
            }
          }
          
          #Now add steps to the sequence going back
          backward <- T #Still chance of adding forward steps
          start_old <- idx
          start_new <- idx - bw #Current first one: we already know it is a valley
          
          while(backward){
            peak_found <- F
            
            if((start_new - freq - period_len) < 1){
              backward <- F #We are at or close to start of dataframe. Cannot add new ones
            }
            else if(sum(df$in_seq[(start_new - freq - period_len):(start_new - 1)]) != 0){
              backward <- F 
              
              #We do not want to overlap with another sequence of movements. Later can add that we may want
              #to merge here // throw a message, but in principle indicates a break at some point,
              #as otherwise we would have detected this one earlier
            }
            else if(df$step_start[start_new - freq] == 1){
              #If enough periodicity: add step going backward
              acf_at_freq <- acf(df$ma_vert[(start_new - freq):(start_old)], lag.max = freq, plot = F)$acf[freq + 1]
              
              if(acf_at_freq > period_thresh){
                step_num_bw <- step_num_bw + 1
      
                df$in_seq[(start_new - freq):(start_new - 1)] <- seq_num
                df$in_step_bw[(start_new - freq):(start_new - 1)] <- step_num_bw
                
                seq_list[[seq_num]]$cor_bw <- append(seq_list[[seq_num]]$cor_bw, acf_at_freq)
                
                peak_found <- T
                
                start_old <- start_new
                start_new <- start_new - freq
              } else{  #If little periodicity/correlation: stop collecting new steps 
                
                backward <- F
                
              }
            } else{ #If not at lag difference of 0, see if at any other lag
              for(k in 1:period_len){
                
                if (df$step_start[start_new - (freq + k*rounding)] == 1){
                  #If enough periodicity: add step going forward
                  #If little periodicity/correlation: stop collecting new steps
                  #If enough periodicity: add step going forward
                  
                  acf_at_freq <- acf(df$ma_vert[(start_new - (freq + k*rounding)):(start_old)], lag.max = freq + k*abs(rounding), plot = F)$acf[floor((freq + freq + k*rounding)/2) + 1]
                  
                  if(acf_at_freq > period_thresh){
                    step_num_bw <- step_num_bw + 1

                    df$in_seq[(start_new - (freq + k*rounding)):(start_new - 1)] <- seq_num
                    df$in_step_bw[(start_new - (freq + k*rounding)):(start_new - 1)] <- step_num_bw
                    
                    seq_list[[seq_num]]$cor_bw <- append(seq_list[[seq_num]]$cor_bw, acf_at_freq)
                    
                    peak_found <- T
                    
                    start_old <- start_new
                    start_new <- start_new - (freq + k*rounding)
                  } else{  #If little periodicity/correlation: stop collecting new steps 
                    
                    backward <- F
                    
                  }                  
                  break
                }
                else if (df$step_start[start_new - (freq - k*rounding)] == 1){
                  #If enough periodicity: add step going forward
                  #If little periodicity/correlation: stop collecting new steps
                  
                  acf_at_freq <- acf(df$ma_vert[(start_new - (freq - k*rounding)):start_old], lag.max = freq + k*abs(rounding), plot = F)$acf[floor((freq + freq - k*rounding)/2) + 1]
                  
                  if(acf_at_freq > period_thresh){
                    step_num_bw <- step_num_bw + 1
                    
                    df$in_seq[(start_new - (freq - k*rounding)):(start_new - 1)] <- seq_num
                    df$in_step_bw[(start_new - (freq - k*rounding)):(start_new - 1)] <- step_num_bw
                    
                    seq_list[[seq_num]]$cor_bw <- append(seq_list[[seq_num]]$cor_bw, acf_at_freq)
                    
                    peak_found <- T
                    
                    start_old <- start_new
                    start_new <- start_new - (freq - k*rounding)
                  } else{  #If little periodicity/correlation: stop collecting new steps 
                    
                    backward <- F
                    
                  }                  
                  break
                }
                
              }
              
              #Here handle case that after this for-loop still no peak found: also
              #stop collecting new steps
              if(peak_found == F){
                backward <- F
              }  
              
            }
          }
          
          #As we are in the situation where we really did have a new sequence, and have changed the
          #df accordingly, we can now add it to our list of sequences
          
          seq_list[[seq_num]]$data <- filter(df, in_seq == seq_num)
        }
      }
    }
  }
  
  
  #Manipulate sequence list!!! Should be a list of dataframes. We do not need to "save" sequences, etc.
  return(list(df, seq_list, seq_num))
}

Rotate_and_classify <- function(seq_list, thresh_direction){
  
  #Cleaning + inverting
  for(seq_num in 1:length(seq_list)){
    
    #Cleaning
    df <- seq_list[[seq_num]]$data
    df <- select(df, time, row, acc_fw, acc_vert, acc_side, in_step_fw, in_step_bw)
    
    df$acc_fw <- df$acc_fw * (seq_list[[seq_num]]$inverted) #Invert data within this sequence
    df$acc_vert <- df$acc_vert * (seq_list[[seq_num]]$inverted) #Invert data within this sequence
    
    seq_list[[seq_num]]$data <- df
  }
  
  #Extra rotation for y: instead of acc_vert use the sequence vector mean
  for(seq_num in 1:length(seq_list)){
    df <- seq_list[[seq_num]]$data
    
    rest_fw <- mean(df$acc_fw)
    rest_vert <- mean(df$acc_vert)
    rest_side <- mean(df$acc_side)
    rest_vec <- c(rest_fw, rest_vert, rest_side)
    
    rot_mat <- rotation_initial(c(rest_fw, rest_vert, rest_side))[[1]]
    
    #Now need to apply this rotation to fw, vert, side to get true vertical acceleration
    df[, 3:5] <- t(rot_mat %*% t(df[, 3:5])) #3:5 is fw, vert, side
    
    #print(mean(df$acc_vert)) #Should be around -1 for all of them!
    #print(mean(df$acc_side)) #Should be around 0 for all!
    #print(mean(df$acc_fw)) #Should be around 0 for all!
    
    #Again, subtract gravity if any left ???
    df$acc_vert <- df$acc_vert - mean(df$acc_vert)
      
    seq_list[[seq_num]]$data <- df
  }
  
  #Rotation horizontal
  for(seq_num in 1:length(seq_list)){
    
    df <- seq_list[[seq_num]]$data

    x_acc <- df$acc_fw
    z_acc <- df$acc_side
    acc_matrix <- cbind(x_acc, z_acc)
    
    cov_matrix <- cov(acc_matrix)
    eig <- eigen(cov_matrix)
    
    forward_vector <- eig$vectors[, 1]
    
    # if(forward_vector[1] < 0 & forward_vector[2] < 0){
    #   print("Negative variant of eigenvector reported. Should not be problematic. If not, remove this if statement")
    #   print(seq_num)
    # }
    if((forward_vector[1])*(forward_vector[2]) < 0) 
      {
      print("Forward direction unexpected. Check. Seq num, main PCA first (x) and second (z) are respectively:")
      print(seq_num)
      print(forward_vector[1])
      print(forward_vector[2])
      
      #We only want to throw them out if it is not that once of them is very close to 0 either. Then we would be going
      #almost straight still and would still be fine
      if(abs((forward_vector[1])*(forward_vector[2])) > thresh_direction){ #Should also be param!
        seq_list[[seq_num]]$class <- 0 #Unlikely to be a walking sequence. 
      }

    }
    
    #Does this still work if the multiplication above is below -1? 
    #We should be able to rotate still if accelerometer is left and somewhat on back instead of front
    angle_rad <- atan2(abs(forward_vector[2]), abs(forward_vector[1]))
    #Abs not necessary here, but possible. Gives same result either way
    #print(angle_rad*180/pi)
    rotation_matrix <- matrix(c(cos(angle_rad), -sin(angle_rad), 
                                sin(angle_rad), cos(angle_rad)), nrow = 2)
    rotated_acc <- acc_matrix %*% t(rotation_matrix)
    
    sideways_acc <- rotated_acc[, 2]
    forward_acc <- rotated_acc[, 1]
    
    df$acc_side <- sideways_acc
    df$acc_fw <- forward_acc
    
    seq_list[[seq_num]]$data <- df
  }
  
  #Classification, either remove or add value how likely it is to be walking to seq_list
  
  return(seq_list)
}

#Temp check!!!
# x_temp <- sequences_walk_1[[1]]$data$acc_fw
# z_temp <- sequences_walk_1[[1]]$data$acc_side
# cbind(x_temp, z_temp) %>% cov() %>% eigen()
# 
# x <- c(1,0,1)
# z <- c(0,1,1)
# acc_matrix <- cbind(x, z)
# angle_rad <- 1.22 #70 deg
# rotation_matrix <- matrix(c(cos(angle_rad), -sin(angle_rad),
#                             sin(angle_rad), cos(angle_rad)), nrow = 2)
# (rotated_acc <- acc_matrix %*% t(rotation_matrix))
# angle_rad <- 0.17 #10 deg
# rotation_matrix <- matrix(c(cos(angle_rad), -sin(angle_rad),
#                             sin(angle_rad), cos(angle_rad)), nrow = 2)
# (rotated_acc <- acc_matrix %*% t(rotation_matrix))

#The hill_dominating_thresh can be improved upon. Sometimes dominated with factor 10 even if still clear second for human eye. Maybe compare to third as well?
Glue_and_classify <- function(seq_list, time_thresh, freq_diff_thresh, min_total, wdw_size, min_highest, allow_small, hill_dominating_thresh, no_ma_fourier, kernel_fourier, ...){
  
  wdw_total <- 2*wdw_size + 1
  
  seq_list <- lapply(seq_list, function(new_seq){
    new_seq$len <- nrow(new_seq$data)
    return(new_seq)
  })
  seq_list <- lapply(seq_list, function(new_seq){
    new_seq$overarch <- 0
    return(new_seq)
  })
  seq_list <- lapply(seq_list, function(new_seq){
    new_seq$movement_type <- "Invalid"
    return(new_seq)
  })
  
  sorted_seq <- seq_list[order(sapply(seq_list, function(x) x$data$time[1]))]
  
  overarch <- numeric(length(sorted_seq))
  num <- 1
  overarch[1] <- num
  sorted_seq[[1]]$overarch <- num
  
  for(j in 2:length(sorted_seq)){
    
    #If too much time between them, we always get a new sequence
    
    time_diff <- (sorted_seq[[j]]$data$time[1] - tail(sorted_seq[[j-1]]$data$time, 1))
    if(as.numeric(time_diff, units = "secs") > time_thresh){
      num <- num + 1
    } #We can also get a new sequence if different frequency: often when going from running to walking or vice versa
    else if(abs(sorted_seq[[j]]$main_freq - sorted_seq[[j-1]]$main_freq) > freq_diff_thresh){
      num <- num + 1 
    }
    overarch[j] <- num
    sorted_seq[[j]]$overarch <- num
  }
  
  
  valid_overarch <- c()
  
  for(num in 1:max(overarch)){
    total <- 0
    highest <- 0
    idc_num <- which(overarch == num)
    
    for(j in idc_num){
      total <- total + sorted_seq[[j]]$len
      highest <- max(highest, sorted_seq[[j]]$len)
    }
    
    #At least one movement sequence of certain size to classify. Add param for size!
    if((total > min_total) & (highest > min_highest)){
      valid_overarch <- append(valid_overarch, num)
    }
  }
  
  for(num in unique(valid_overarch)){
    
    freq_avg <- c()
    
    idc_num <- which(overarch == num)
    
    for(j in idc_num){
      if(sorted_seq[[j]]$len > wdw_total){
        
        middle_idx <- (sorted_seq[[j]]$len %/% 2) + 1
        start_idx <- middle_idx - wdw_size
        end_idx <- middle_idx + wdw_size
        
        spec <- sorted_seq[[j]]$data$acc_fw[start_idx:end_idx] %>% energy_kernel(., no_ma_fourier, kernel_fourier, ...)
        main_freq_avg <- (capture_hills(spec, 2)[[2]][[1]] + capture_hills(spec, 2)[[2]][[2]])/2
      
        #If the second (or first) frequency is very high, it indicates some shift in mean 
        #just a few times. In that case, we should probably just label the whole movement as invalid. However,
        #in practice have seen that this must always be walking. Makes sense ...
        if(capture_hills(spec, 2)[[2]][[1]] > 50 | capture_hills(spec, 2)[[2]][[2]] > 50){ #50 can be parameter, but this is quick fix anyway
          main_freq_avg <- 0 #Such that it always get labelled as walking, not cycling
        }
        
        main_freq_dom <- main_freq_dom <- capture_hills(spec,2)[[3]][[1]]/capture_hills(spec,2)[[3]][[2]]
        
        if((main_freq_avg > sorted_seq[[j]]$main_freq) & (main_freq_dom < hill_dominating_thresh)){
          freq_avg <- append(freq_avg, T)
        }
        else{
          freq_avg <- append(freq_avg, F)
        } 
        
      }
      else if((sorted_seq[[j]]$len > min_highest) & allow_small == T){
        
        middle_idx <- (sorted_seq[[j]]$len %/% 2) + 1
        start_idx <- middle_idx - (min_highest %/% 2)
        end_idx <- middle_idx + (min_highest %/% 2)
        #Cannot take MA here or hills merge, but sometimes just plain bad luck
        spec <- sorted_seq[[j]]$data$acc_fw[start_idx:end_idx] %>% energy_kernel(., 1, ma_trunc)
        #The second frequency being double the main one, thus on average being above main one, is indication of cycling
        main_freq_avg <- (capture_hills(spec, 2)[[2]][[1]] + capture_hills(spec, 2)[[2]][[2]])/2
        
        #If the second (or first) frequency is very high, it indicates some shift in mean 
        #just a few times. In that case, we should probably just label the whole movement as invalid. However,
        #in practice have seen that this must always be walking. Makes sense ...
        if(capture_hills(spec, 2)[[2]][[1]] > 50 | capture_hills(spec, 2)[[2]][[2]] > 50){ #50 can be parameter, but this is quick fix anyway
          main_freq_avg <- 0 #Such that it always get labelled as walking, not cycling
        }
        #The second frequency being somewhat important in the forward direction is indication of cycling as well
        #So needs to happen. This needs to be double checked, because the first event can fail and then this is often applicable
        #Also works something well if step qualified as double step etc. ???
        main_freq_dom <- capture_hills(spec,2)[[3]][[1]]/capture_hills(spec,2)[[3]][[2]]
        
        if((main_freq_avg > sorted_seq[[j]]$main_freq) & (main_freq_dom < hill_dominating_thresh)){
          freq_avg <- append(freq_avg, T)
        }
        else{
          freq_avg <- append(freq_avg, F)
        } 
      }
    }
    
    
    if(all(freq_avg)){
      for(j in idc_num){
        sorted_seq[[j]]$movement_type <- "Cycle"
      }
    }
    else if(all(!freq_avg)){
      for(j in idc_num){
        sorted_seq[[j]]$movement_type <- "Walk"
      }
    }
    else{
      for(j in idc_num){
        sorted_seq[[j]]$movement_type <- "Mixed"
      }
    }
  }
  
  return(sorted_seq)
}

Fast_big_window_manipulation <- function(df, wdw_size, ma_data, ma_energy, upright_thresh, abs_acc_thresh, kernel_fourier, ...){
  
  N <- nrow(df)
  
  frequencies <- numeric(N)
  relative_frac <- numeric(N)
  absolute_frac <- numeric(N)
  frequencies_2 <- numeric(N)
  relative_frac_2 <- numeric(N)
  absolute_frac_2 <- numeric(N)
  
  upright_inverted <- numeric(N) 
  
  acc_vert <- numeric(N) 
  acc_side <- numeric(N) 
  acc_fw <- numeric(N)
  
  df$row <- seq(1:N)
  
  for(k in 1:N){
    if(k%%1500 == 0){
      print(paste("Progress: ", k/N))
    }
    
    start <- max(1, k - wdw_size)
    end <- min(N, k + wdw_size)
    window <- df[(start):(end),]
    rest_x <- mean(window$X)
    rest_y <- mean(window$Y)
    rest_z <- mean(window$Z)
    rest_vec <- c(rest_x, rest_y, rest_z)
    rot_mat <- rotation_initial(c(rest_x, rest_y, rest_z))[[1]]
    window[, 2:4] <- t(rot_mat %*% t(window[, 2:4]))
    
    
    if(mean(window[, 3]) > upright_thresh){
      upright_inverted[k] <- -1
    }
    else if(mean(window[,3] < -upright_thresh)){
      upright_inverted[k] <- 1
    }
    else{
      upright_inverted[k] <- 0
    }
    
    rot_Y <- t(rot_mat %*% t(df[k, 2:4]))[2] #y coordinate in that point
    rot_X <- t(rot_mat %*% t(df[k, 2:4]))[1] #horizontal one direction
    rot_Z <- t(rot_mat %*% t(df[k, 2:4]))[3] #horizontal orthogonal direction
    
    acc_vert[k] <- rot_Y
    acc_fw[k] <- rot_X
    acc_side[k] <- rot_Z
  }
  
  df$ma_Y <- ma_trunc(df$Y, n = ma_data)
  
  acc_vert <- acc_vert - mean(acc_vert) #Subtracting gravity 

  df$acc_vert <- acc_vert
  df$ma_vert <-  ma_trunc(df$acc_vert, n = ma_data)
  
  df$acc_side <- acc_side
  df$acc_fw <- acc_fw
  
  df$upright_inverted <- upright_inverted
  
  df <- df %>%
    mutate(
      step_valley = ifelse(ma_vert < lag(ma_vert, default = Inf) & 
                             ma_vert < lead(ma_vert, default = Inf) & 
                             ma_vert < -abs_acc_thresh, 1, 0) #Changed: now just ''enough absolute acceleration'' (gravity helps, both if worn inverted and normal)
    )
  df <- df %>%
    mutate(
      step_peak = ifelse(ma_vert > lag(ma_vert, default = Inf) & 
                           ma_vert > lead(ma_vert, default = Inf) & 
                           ma_vert > abs_acc_thresh, 1, 0) #Changed: now just ''enough absolute acceleration'' (gravity helps, both if worn inverted and normal)
    )

  for(k in 1:N){
    
    if((df$step_peak[k] == 1) | (df$step_valley[k] == 1)){
      
      start <- max(1, k - wdw_size)
      end <- min(N, k + wdw_size)
      window <- df[(start):(end),]
      
      rest_x <- mean(window$X)
      rest_y <- mean(window$Y)
      rest_z <- mean(window$Z)
      
      rest_vec <- c(rest_x, rest_y, rest_z)
      rot_mat <- rotation_initial(c(rest_x, rest_y, rest_z))[[1]]
      window[, 2:4] <- t(rot_mat %*% t(window[, 2:4]))
      
      spec <- window$Y %>% energy_kernel(., ma_energy, kernel_fourier, ...)
      
      hill_info <- capture_hills(spec, 2)
      frequencies[k] <- hill_info[[2]][[1]]
      relative_frac[k] <- hill_info[[3]][[1]]
      absolute_frac[k] <- hill_info[[4]][[1]]
      
    }
    else{
      frequencies[k] <- 0
      relative_frac[k] <- 0
      absolute_frac[k] <- 0
    }
  }
  
  df$frequencies <- frequencies
  df$relative_frac <- relative_frac
  df$absolute_frac <- absolute_frac
  
  df$in_seq <- numeric(N) 
  df$in_step_fw <- numeric(N)
  df$in_step_bw <- numeric(N)
  
  return(df)
}