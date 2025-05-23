rescale_improved <- function(step, size){
  n <- length(step) + 1
  new_indices <- seq(1, n, length.out = size + 1)
  return(approx(seq(1, n), c(step, step[1]), xout = new_indices)$y[1:size])
}

ma_cyclic <- function(vec,n){
  len <- length(vec)

  half <- (n - 1)/2
  
  # Create output vector
  result <- numeric(len)
  
  for (i in 1:len) {
    # Determine the indices to include in the moving average, wrapping around using modulo
    indices <- ((i - half - 1):(i + half - 1)) %% len + 1
    result[i] <- mean(vec[indices])
  }
  
  return(result)
}

count_local_maxima_cyclic <- function(vec) {
  n <- length(vec)
  
  prev <- c(vec[n], vec[1:(n - 1)])
  nxt <- c(vec[2:n], vec[1])
  
  is_max <- vec > prev & vec > nxt
  
  return(sum(is_max))
}

count_local_minima_cyclic <- function(vec) {
  n <- length(vec)
  
  prev <- c(vec[n], vec[1:(n - 1)])
  nxt <- c(vec[2:n], vec[1])
  
  is_min <- vec < prev & vec < nxt
  
  return(sum(is_min))
}

df_all_from_mvm_alt_nofilter_improved <- function(data, n, start_function){
  all_fw_steps <- list()
  all_vert_steps <- list()
  all_side_steps <- list()
  
  step_data <- list(mvm = c())
  
  mvm_num <- 0
  
  for(mvm in data){
    mvm_fw_steps <- list()
    mvm_vert_steps <- list()
    mvm_side_steps <- list()
    
    end_bw <- max(mvm$data$in_step_bw)
    end_fw <- max(mvm$data$in_step_fw)
    
    for(j in 1:end_bw){
      step <- filter(mvm$data, in_step_bw == j)
      
      new_fw <- rescale_improved(step$acc_fw, n)
      new_vert <- rescale_improved(step$acc_vert, n)
      new_side <- rescale_improved(step$acc_side, n)
      
      mvm_fw_steps <- append(mvm_fw_steps, list(new_fw))
      mvm_vert_steps <- append(mvm_vert_steps, list(new_vert))
      mvm_side_steps <- append(mvm_side_steps, list(new_side))
    }
    
    for(j in 1:end_fw){
      step <- filter(mvm$data, in_step_fw == j)
      
      new_fw <- rescale_improved(step$acc_fw, n)
      new_vert <- rescale_improved(step$acc_vert, n)
      new_side <- rescale_improved(step$acc_side, n)
      
      mvm_fw_steps <- append(mvm_fw_steps, list(new_fw))
      mvm_vert_steps <- append(mvm_vert_steps, list(new_vert))
      mvm_side_steps <- append(mvm_side_steps, list(new_side))
    }
    
    mvm_fw_avg <- numeric(n)
    mvm_vert_avg <- numeric(n)
    mvm_side_avg <- numeric(n)
    
    if(length(mvm_fw_steps) > 0){
      for(j in 1:length(mvm_fw_steps)){
        mvm_fw_avg <- mvm_fw_avg + mvm_fw_steps[[j]]
        mvm_vert_avg <- mvm_vert_avg + mvm_vert_steps[[j]]
        mvm_side_avg <- mvm_side_avg + mvm_side_steps[[j]]
      }
      mvm_fw_avg <- mvm_fw_avg/length(mvm_fw_steps)
      mvm_vert_avg <- mvm_vert_avg/length(mvm_fw_steps)
      mvm_side_avg <- mvm_side_avg/length(mvm_fw_steps)
      
      if(mvm_num > 0){
        start_index <- start_function(list(mvm_fw_avg, 
                                           mvm_vert_avg, 
                                           mvm_side_avg,
                                           avg_fw_steps_sofar,
                                           avg_vert_steps_sofar,
                                           avg_side_steps_sofar)) 
      }
      else{
        start_index <- which.max(mvm_vert_avg)
      }
      
      #Now we cycle every rescaled step in mvm_steps individually and add them to the df
      for(step in mvm_fw_steps){
        if(start_index != 1){
          step <-  c(step[start_index:n], step[1:(start_index-1)])
        }
        all_fw_steps <- append(all_fw_steps, list(step))
        step_data$mvm <- append(step_data$mvm, mvm_num)
      }
      for(step in mvm_vert_steps){
        if(start_index != 1){
          step <-  c(step[start_index:n], step[1:(start_index-1)])
        }
        all_vert_steps <- append(all_vert_steps, list(step))
      }
      for(step in mvm_side_steps){
        if(start_index != 1){
          step <-  c(step[start_index:n], step[1:(start_index-1)])
        }
        all_side_steps <- append(all_side_steps, list(step))
      }
    }
    
    avg_fw_steps_sofar <- numeric(n)
    avg_vert_steps_sofar <- numeric(n)
    avg_side_steps_sofar <- numeric(n)
    
    #Compute average so far from e.g. all_fw_steps we have right now. Used in next cycle
    if(length(all_fw_steps) > 0){
      for(i in 1:length(all_fw_steps)){
        for(j in 1:n){
          avg_fw_steps_sofar[j] <- avg_fw_steps_sofar[j] + all_fw_steps[[i]][j]
          avg_vert_steps_sofar[j] <- avg_vert_steps_sofar[j] + all_vert_steps[[i]][j]
          avg_side_steps_sofar[j] <- avg_side_steps_sofar[j] + all_side_steps[[i]][j]
        }
      }
      avg_fw_steps_sofar <- avg_fw_steps_sofar/length(all_fw_steps)
      avg_vert_steps_sofar <- avg_vert_steps_sofar/length(all_fw_steps)
      avg_side_steps_sofar <- avg_side_steps_sofar/length(all_fw_steps)
      
      mvm_num <- mvm_num + 1
    }
  }
  fw_df <- rbindlist(lapply(all_fw_steps, as.list))
  vert_df <- rbindlist(lapply(all_vert_steps, as.list))
  side_df <- rbindlist(lapply(all_side_steps, as.list))
  #side_df <- as.data.frame(do.call(rbind, all_steps))
  return(list(fw_df, vert_df, side_df, step_data))
}


#The properties are either how much and in what way they may differ, or 
#some objective min/max etc. Probably the former to more efficiently compute a type for all steps
df_all_from_mvm_alt_filter_improved <- function(data, n, start_function,
                                                min_freq = 8, max_freq = 11,
                                                min_step_len = 15, max_step_len = 23,
                                                ma_fw = 5, 
                                                no_peaks_fw = 2,
                                                no_valleys_fw = 2,
                                                ma_vert = 5,
                                                no_peaks_vert = 2,
                                                no_valleys_vert = 2,
                                                variance = Inf, sumabs = Inf,
                                                absnegvert = Inf, absposvert = Inf,
                                                absnegfw = Inf, absposfw = Inf){
  all_fw_steps <- list()
  all_vert_steps <- list()
  all_side_steps <- list()
  
  step_data <- list(mvm = c())
  
  mvm_num <- 0
  
  for(mvm in data){
    if(mvm$main_freq >= min_freq & mvm$main_freq <= max_freq){
    
    mvm_fw_steps <- list()
    mvm_vert_steps <- list()
    mvm_side_steps <- list()
    
    end_bw <- max(mvm$data$in_step_bw)
    end_fw <- max(mvm$data$in_step_fw)
    
    for(j in 1:end_bw){
      step <- filter(mvm$data, in_step_bw == j)
      if(length(step$acc_fw) >= min_step_len & length(step$acc_fw) <= max_step_len){
        if(count_local_maxima_cyclic(ma_cyclic(step$acc_fw, ma_fw)) == no_peaks_fw){ 
          if(count_local_minima_cyclic(ma_cyclic(step$acc_fw, ma_fw)) == no_valleys_fw){ 
            if(count_local_maxima_cyclic(ma_cyclic(step$acc_vert, ma_vert)) == no_peaks_vert){ 
              if(count_local_minima_cyclic(ma_cyclic(step$acc_vert, ma_vert)) == no_valleys_vert){ 
      new_fw <- rescale_improved(step$acc_fw, n)
      new_vert <- rescale_improved(step$acc_vert, n)
      new_side <- rescale_improved(step$acc_side, n)
      
      mvm_fw_steps <- append(mvm_fw_steps, list(new_fw))
      mvm_vert_steps <- append(mvm_vert_steps, list(new_vert))
      mvm_side_steps <- append(mvm_side_steps, list(new_side))
              }
            }
          }
        }
      }
    }
    
    for(j in 1:end_fw){
      step <- filter(mvm$data, in_step_fw == j)
            if(length(step$acc_fw) >= min_step_len & length(step$acc_fw) <= max_step_len){
        if(count_local_maxima_cyclic(ma_cyclic(step$acc_fw, ma_fw)) == no_peaks_fw){ 
          if(count_local_minima_cyclic(ma_cyclic(step$acc_fw, ma_fw)) == no_valleys_fw){ 
            if(count_local_maxima_cyclic(ma_cyclic(step$acc_vert, ma_vert)) == no_peaks_vert){ 
              if(count_local_minima_cyclic(ma_cyclic(step$acc_vert, ma_vert)) == no_valleys_vert){ 
      
      new_fw <- rescale_improved(step$acc_fw, n)
      new_vert <- rescale_improved(step$acc_vert, n)
      new_side <- rescale_improved(step$acc_side, n)
      
      mvm_fw_steps <- append(mvm_fw_steps, list(new_fw))
      mvm_vert_steps <- append(mvm_vert_steps, list(new_vert))
      mvm_side_steps <- append(mvm_side_steps, list(new_side))
              }
            }
          }
        }
      }
    }
    
    mvm_fw_avg <- numeric(n)
    mvm_vert_avg <- numeric(n)
    mvm_side_avg <- numeric(n)
    
    if(length(mvm_fw_steps) > 0){
      for(j in 1:length(mvm_fw_steps)){
        mvm_fw_avg <- mvm_fw_avg + mvm_fw_steps[[j]]
        mvm_vert_avg <- mvm_vert_avg + mvm_vert_steps[[j]]
        mvm_side_avg <- mvm_side_avg + mvm_side_steps[[j]]
      }
      mvm_fw_avg <- mvm_fw_avg/length(mvm_fw_steps)
      mvm_vert_avg <- mvm_vert_avg/length(mvm_fw_steps)
      mvm_side_avg <- mvm_side_avg/length(mvm_fw_steps)
      
      if(mvm_num > 0){
        start_index <- start_function(list(mvm_fw_avg, 
                                           mvm_vert_avg, 
                                           mvm_side_avg,
                                           avg_fw_steps_sofar,
                                           avg_vert_steps_sofar,
                                           avg_side_steps_sofar)) 
      }
      else{
        start_index <- which.max(mvm_vert_avg)
      }
      
      #Now we cycle every rescaled step in mvm_steps individually and add them to the df
      for(step in mvm_fw_steps){
        if(start_index != 1){
          step <-  c(step[start_index:n], step[1:(start_index-1)])
        }
        all_fw_steps <- append(all_fw_steps, list(step))
        step_data$mvm <- append(step_data$mvm, mvm_num)
      }
      for(step in mvm_vert_steps){
        if(start_index != 1){
          step <-  c(step[start_index:n], step[1:(start_index-1)])
        }
        all_vert_steps <- append(all_vert_steps, list(step))
      }
      for(step in mvm_side_steps){
        if(start_index != 1){
          step <-  c(step[start_index:n], step[1:(start_index-1)])
        }
        all_side_steps <- append(all_side_steps, list(step))
      }
    }
    
    avg_fw_steps_sofar <- numeric(n)
    avg_vert_steps_sofar <- numeric(n)
    avg_side_steps_sofar <- numeric(n)
    
    #Compute average so far from e.g. all_fw_steps we have right now. Used in next cycle
    if(length(all_fw_steps) > 0){
      for(i in 1:length(all_fw_steps)){
        for(j in 1:n){
          avg_fw_steps_sofar[j] <- avg_fw_steps_sofar[j] + all_fw_steps[[i]][j]
          avg_vert_steps_sofar[j] <- avg_vert_steps_sofar[j] + all_vert_steps[[i]][j]
          avg_side_steps_sofar[j] <- avg_side_steps_sofar[j] + all_side_steps[[i]][j]
        }
      }
      avg_fw_steps_sofar <- avg_fw_steps_sofar/length(all_fw_steps)
      avg_vert_steps_sofar <- avg_vert_steps_sofar/length(all_fw_steps)
      avg_side_steps_sofar <- avg_side_steps_sofar/length(all_fw_steps)
      
      mvm_num <- mvm_num + 1
    }
  }
}
  fw_df <- rbindlist(lapply(all_fw_steps, as.list))
  vert_df <- rbindlist(lapply(all_vert_steps, as.list))
  side_df <- rbindlist(lapply(all_side_steps, as.list))
  #side_df <- as.data.frame(do.call(rbind, all_steps))
  return(list(fw_df, vert_df, side_df, step_data))
}

df_all_from_mvm_alt_nofilter_improved_nocycle <- function(data, n){
  all_fw_steps <- list()
  all_vert_steps <- list()
  all_side_steps <- list()
  
  step_data <- list(mvm = c())
  
  mvm_num <- 0
  
  for(mvm in data){
    if(mvm$main_freq >= min_freq & mvm$main_freq <= max_freq){
      
      mvm_fw_steps <- list()
      mvm_vert_steps <- list()
      mvm_side_steps <- list()
      
      end_bw <- max(mvm$data$in_step_bw)
      end_fw <- max(mvm$data$in_step_fw)
      
      for(j in 1:end_bw){
        step <- filter(mvm$data, in_step_bw == j)
        
                  new_fw <- rescale_improved(step$acc_fw, n)
                  new_vert <- rescale_improved(step$acc_vert, n)
                  new_side <- rescale_improved(step$acc_side, n)
                  
                  mvm_fw_steps <- append(mvm_fw_steps, list(new_fw))
                  mvm_vert_steps <- append(mvm_vert_steps, list(new_vert))
                  mvm_side_steps <- append(mvm_side_steps, list(new_side))
      }
      
      for(j in 1:end_fw){
        step <- filter(mvm$data, in_step_fw == j)
        
                  
                  new_fw <- rescale_improved(step$acc_fw, n)
                  new_vert <- rescale_improved(step$acc_vert, n)
                  new_side <- rescale_improved(step$acc_side, n)
                  
                  mvm_fw_steps <- append(mvm_fw_steps, list(new_fw))
                  mvm_vert_steps <- append(mvm_vert_steps, list(new_vert))
                  mvm_side_steps <- append(mvm_side_steps, list(new_side))
      }
      
      for(step in mvm_fw_steps){
        all_fw_steps <- append(all_fw_steps, list(step))
        step_data$mvm <- append(step_data$mvm, mvm_num)
      }
      for(step in mvm_vert_steps){
        all_vert_steps <- append(all_vert_steps, list(step))
      }
      for(step in mvm_side_steps){
        all_side_steps <- append(all_side_steps, list(step))
      }
      mvm_num <- mvm_num + 1
    }
  }
  
  fw_df <- rbindlist(lapply(all_fw_steps, as.list))
  vert_df <- rbindlist(lapply(all_vert_steps, as.list))
  side_df <- rbindlist(lapply(all_side_steps, as.list))
  
  return(list(fw_df, vert_df, side_df, step_data))
  
}

#The properties are either how much and in what way they may differ, or 
#some objective min/max etc. Probably the former to more efficiently compute a type for all steps
df_all_from_mvm_alt_filter_improved_nocycle <- function(data, n,
                                                min_freq = 8, max_freq = 11,
                                                min_step_len = 15, max_step_len = 23,
                                                ma_fw = 5, 
                                                no_peaks_fw = 2,
                                                no_valleys_fw = 2,
                                                ma_vert = 5,
                                                no_peaks_vert = 2,
                                                no_valleys_vert = 2,
                                                variance = Inf, sumabs = Inf,
                                                absnegvert = Inf, absposvert = Inf,
                                                absnegfw = Inf, absposfw = Inf){
  all_fw_steps <- list()
  all_vert_steps <- list()
  all_side_steps <- list()
  
  step_data <- list(mvm = c())
  
  mvm_num <- 0
  
  for(mvm in data){
    if(mvm$main_freq >= min_freq & mvm$main_freq <= max_freq){
      
      mvm_fw_steps <- list()
      mvm_vert_steps <- list()
      mvm_side_steps <- list()
      
      end_bw <- max(mvm$data$in_step_bw)
      end_fw <- max(mvm$data$in_step_fw)
      
      for(j in 1:end_bw){
        step <- filter(mvm$data, in_step_bw == j)
        if(length(step$acc_fw) >= min_step_len & length(step$acc_fw) <= max_step_len){
          if(count_local_maxima_cyclic(ma_cyclic(step$acc_fw, ma_fw)) == no_peaks_fw){ 
            if(count_local_minima_cyclic(ma_cyclic(step$acc_fw, ma_fw)) == no_valleys_fw){ 
              if(count_local_maxima_cyclic(ma_cyclic(step$acc_vert, ma_vert)) == no_peaks_vert){ 
                if(count_local_minima_cyclic(ma_cyclic(step$acc_vert, ma_vert)) == no_valleys_vert){ 
                  new_fw <- rescale_improved(step$acc_fw, n)
                  new_vert <- rescale_improved(step$acc_vert, n)
                  new_side <- rescale_improved(step$acc_side, n)
                  
                  mvm_fw_steps <- append(mvm_fw_steps, list(new_fw))
                  mvm_vert_steps <- append(mvm_vert_steps, list(new_vert))
                  mvm_side_steps <- append(mvm_side_steps, list(new_side))
                }
              }
            }
          }
        }
      }
      
      for(j in 1:end_fw){
        step <- filter(mvm$data, in_step_fw == j)
        if(length(step$acc_fw) >= min_step_len & length(step$acc_fw) <= max_step_len){
          if(count_local_maxima_cyclic(ma_cyclic(step$acc_fw, ma_fw)) == no_peaks_fw){ 
            if(count_local_minima_cyclic(ma_cyclic(step$acc_fw, ma_fw)) == no_valleys_fw){ 
              if(count_local_maxima_cyclic(ma_cyclic(step$acc_vert, ma_vert)) == no_peaks_vert){ 
                if(count_local_minima_cyclic(ma_cyclic(step$acc_vert, ma_vert)) == no_valleys_vert){ 
                  
                  new_fw <- rescale_improved(step$acc_fw, n)
                  new_vert <- rescale_improved(step$acc_vert, n)
                  new_side <- rescale_improved(step$acc_side, n)
                  
                  mvm_fw_steps <- append(mvm_fw_steps, list(new_fw))
                  mvm_vert_steps <- append(mvm_vert_steps, list(new_vert))
                  mvm_side_steps <- append(mvm_side_steps, list(new_side))
                }
              }
            }
          }
        }
      }
      
      for(step in mvm_fw_steps){
        all_fw_steps <- append(all_fw_steps, list(step))
        step_data$mvm <- append(step_data$mvm, mvm_num)
      }
      for(step in mvm_vert_steps){
        all_vert_steps <- append(all_vert_steps, list(step))
      }
      for(step in mvm_side_steps){
        all_side_steps <- append(all_side_steps, list(step))
      }
      mvm_num <- mvm_num + 1
    }
    }
    
    fw_df <- rbindlist(lapply(all_fw_steps, as.list))
    vert_df <- rbindlist(lapply(all_vert_steps, as.list))
    side_df <- rbindlist(lapply(all_side_steps, as.list))
    
    return(list(fw_df, vert_df, side_df, step_data))
}
