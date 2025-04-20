#Key function, which takes start functions that compare steps to average so far
df_all_from_mvm_alt <- function(data, n, start_function, filter_function, filter_par){
  all_fw_steps <- list()
  all_vert_steps <- list()
  all_side_steps <- list()
  
  #Also keep a list of which movement and which overarch each step is a part
  step_data <- list(mvm = c(), overarch = c())
  
  mvm_num <- 0
  overarch_num <- 0
  overarch_current <- -1 #In other words, never equal at the start
  
  for(mvm in data){
    mvm_fw_steps <- list()
    mvm_vert_steps <- list()
    mvm_side_steps <- list()
    
    if(mvm$movement_type == "Walk" & filter_function(mvm, filter_par)){
      
      if(mvm$overarch != overarch_current){
        overarch_current <- mvm$overarch
        overarch_num <- overarch_num + 1
      }
      end_bw <- max(mvm$data$in_step_bw)
      end_fw <- max(mvm$data$in_step_fw)
      
      for(j in 1:end_bw){
        step <- filter(mvm$data, in_step_bw == j)
        
        new_fw <- rescale(step$acc_fw, n)
        new_vert <- rescale(step$acc_vert, n)
        new_side <- rescale(step$acc_side, n)
        
        mvm_fw_steps <- append(mvm_fw_steps, list(new_fw))
        mvm_vert_steps <- append(mvm_vert_steps, list(new_vert))
        mvm_side_steps <- append(mvm_side_steps, list(new_side))
      }
      
      for(j in 1:end_fw){
        step <- filter(mvm$data, in_step_fw == j)
        
        new_fw <- rescale(step$acc_fw, n)
        new_vert <- rescale(step$acc_vert, n)
        new_side <- rescale(step$acc_side, n)
        
        mvm_fw_steps <- append(mvm_fw_steps, list(new_fw))
        mvm_vert_steps <- append(mvm_vert_steps, list(new_vert))
        mvm_side_steps <- append(mvm_side_steps, list(new_side))
      }
      
      mvm_fw_avg <- numeric(n)
      mvm_vert_avg <- numeric(n)
      mvm_side_avg <- numeric(n)
    }
    
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
        step_data$overarch <- append(step_data$overarch, overarch_num)
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

#Key function!
df_all_from_mvm <- function(data, n, start_function, filter_function, filter_par){
  all_fw_steps <- list()
  all_vert_steps <- list()
  all_side_steps <- list()
  
  for(mvm in data){
    mvm_fw_steps <- list()
    mvm_vert_steps <- list()
    mvm_side_steps <- list()
    
    if(mvm$movement_type == "Walk" & filter_function(mvm, filter_par)){
      
      end_bw <- max(mvm$data$in_step_bw)
      end_fw <- max(mvm$data$in_step_fw)
      
      for(j in 1:end_bw){
        step <- filter(mvm$data, in_step_bw == j)
        
        new_fw <- rescale(step$acc_fw, n)
        new_vert <- rescale(step$acc_vert, n)
        new_side <- rescale(step$acc_side, n)
        
        mvm_fw_steps <- append(mvm_fw_steps, list(new_fw))
        mvm_vert_steps <- append(mvm_vert_steps, list(new_vert))
        mvm_side_steps <- append(mvm_side_steps, list(new_side))
      }
      
      for(j in 1:end_fw){
        step <- filter(mvm$data, in_step_fw == j)
        
        new_fw <- rescale(step$acc_fw, n)
        new_vert <- rescale(step$acc_vert, n)
        new_side <- rescale(step$acc_side, n)
        
        mvm_fw_steps <- append(mvm_fw_steps, list(new_fw))
        mvm_vert_steps <- append(mvm_vert_steps, list(new_vert))
        mvm_side_steps <- append(mvm_side_steps, list(new_side))
      }
      
      mvm_fw_avg <- numeric(n)
      mvm_vert_avg <- numeric(n)
      mvm_side_avg <- numeric(n)
    }
    
    if(length(mvm_fw_steps) > 0){
      for(j in 1:length(mvm_fw_steps)){
        mvm_fw_avg <- mvm_fw_avg + mvm_fw_steps[[j]]
        mvm_vert_avg <- mvm_vert_avg + mvm_vert_steps[[j]]
        mvm_side_avg <- mvm_side_avg + mvm_side_steps[[j]]
      }
      start_index <- start_function(list(mvm_fw_avg, mvm_vert_avg, mvm_side_avg))
      #Now we cycle every rescaled step in mvm_steps individually and add them to the df
      for(step in mvm_fw_steps){
        if(start_index != 1){
          step <-  c(step[start_index:n], step[1:(start_index-1)])
        }
        all_fw_steps <- append(all_fw_steps, list(step))
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
  }
  fw_df <- rbindlist(lapply(all_fw_steps, as.list))
  vert_df <- rbindlist(lapply(all_vert_steps, as.list))
  side_df <- rbindlist(lapply(all_side_steps, as.list))
  #side_df <- as.data.frame(do.call(rbind, all_steps))
  return(list(fw_df, vert_df, side_df))
}

cycle_to_min <- function(step) {
  min_index <- which.min(step$acc_vert)  
  n <- nrow(step)
  
  if(min_index != 1){
    step$acc_vert <-  c(step$acc_vert[min_index:n], step$acc_vert[1:(min_index-1)])  
    step$acc_fw <-  c(step$acc_fw[min_index:n], step$acc_fw[1:(min_index-1)]) 
    step$acc_side <-  c(step$acc_side[min_index:n], step$acc_side[1:(min_index-1)])  
  }
  return(step)
}

rescale <- function(step, size){
  n <- length(step)
  new_indices <- seq(1, n, length.out = size)
  return(approx(seq(1, n), step, xout = new_indices)$y)
}

average_step <- function(data, n, filter_function, filter_par){
  extracted_steps <- list()
  
  for(mvm in data){
    if(mvm$movement_type == "Walk" & filter_function(mvm, filter_par)){
      end_bw <- max(mvm$data$in_step_bw)
      end_fw <- max(mvm$data$in_step_fw)
      
      for(j in 1:end_bw){
        step <- filter(mvm$data, in_step_bw == j)
        new_step <- cycle_to_min(step)
        new_fw <- rescale(new_step$acc_fw, n)
        new_vert <- rescale(new_step$acc_vert, n)
        new_side <- rescale(new_step$acc_side, n)
        
        extracted_steps <- append(extracted_steps, list(list(new_fw, new_vert, new_side)))
        
      }
      
      for(j in 1:end_fw){
        step <- filter(mvm$data, in_step_fw == j)
        
        new_step <- cycle_to_min(step)
        new_fw <- rescale(new_step$acc_fw, n)
        new_vert <- rescale(new_step$acc_vert, n)
        new_side <- rescale(new_step$acc_side, n)
        
        extracted_steps <- append(extracted_steps, list(list(new_fw, new_vert, new_side)))
      }
    }
  }
  avg_fw <- numeric(n)  
  avg_vert <- numeric(n)
  avg_side <- numeric(n)
  
  if(length(extracted_steps) > 0){
    for(j in 1:length(extracted_steps)){
      avg_fw <- avg_fw + extracted_steps[[j]][[1]]
      avg_vert <- avg_vert + extracted_steps[[j]][[2]]
      avg_side <- avg_side + extracted_steps[[j]][[3]]
    }

    avg_fw <- avg_fw/length(extracted_steps)
    avg_vert <- avg_vert/length(extracted_steps)
    avg_side <- avg_side/length(extracted_steps)

    return(list(avg_fw, avg_vert, avg_side, length(extracted_steps)))
  }
  else{
    return(c(0,0,0,0))
  }
}

cycle_to_min_mvm <- function(mvm, n, filter_function, filter_par) {
  
  extracted_steps <- list()
  
  if(mvm$movement_type == "Walk" & filter_function(mvm, filter_par)){
    
    end_bw <- max(mvm$data$in_step_bw)
    end_fw <- max(mvm$data$in_step_fw)
    
    for(j in 1:end_bw){
      step <- filter(mvm$data, in_step_bw == j)
      new_fw <- rescale(step$acc_fw, n)
      new_vert <- rescale(step$acc_vert, n)
      new_side <- rescale(step$acc_side, n)
      
      extracted_steps <- append(extracted_steps, list(list(new_fw, new_vert, new_side)))
    }
    
    for(j in 1:end_fw){
      step <- filter(mvm$data, in_step_fw == j)
      
      new_fw <- rescale(step$acc_fw, n)
      new_vert <- rescale(step$acc_vert, n)
      new_side <- rescale(step$acc_side, n)
      
      extracted_steps <- append(extracted_steps, list(list(new_fw, new_vert, new_side)))
    }
    
    avg_fw <- numeric(n)
    avg_vert <- numeric(n)
    avg_side <- numeric(n)
    
  }
  
  if(length(extracted_steps) > 0){
    for(j in 1:length(extracted_steps)){
      avg_fw <- avg_fw + extracted_steps[[j]][[1]]
      avg_vert <- avg_vert + extracted_steps[[j]][[2]]
      avg_side <- avg_side + extracted_steps[[j]][[3]]
    }
    
    avg_fw <- avg_fw/length(extracted_steps)
    avg_vert <- avg_vert/length(extracted_steps)
    avg_side <- avg_side/length(extracted_steps)
    
    min_index <- which.min(avg_vert)
    
    if(min_index != 1){
      avg_fw <-  c(avg_fw[min_index:n], avg_fw[1:(min_index-1)])  
      avg_vert <-  c(avg_vert[min_index:n], avg_vert[1:(min_index-1)]) 
      avg_side <-  c(avg_side[min_index:n], avg_side[1:(min_index-1)])  
    }
    return(list(avg_fw, avg_vert, avg_side, length(extracted_steps)))
  }
  else{
    return(list(0,0,0,0)) 
  }
}

average_step_from_mvm <- function(data, n, filter_function, filter_par) {
  
  new_avg_fw <- numeric(n)
  new_avg_vert <- numeric(n)
  new_avg_side <- numeric(n)
  tot_steps <- 0
  
  for(mvm in data){
    averages <- cycle_to_min_mvm(mvm, n, filter_function, filter_par)
    if(averages[[4]] > 0){
      new_avg_fw <- new_avg_fw + averages[[1]]*averages[[4]]
      new_avg_vert <- new_avg_vert + averages[[2]]*averages[[4]]
      new_avg_side <- new_avg_side + averages[[3]]*averages[[4]]
      tot_steps <- tot_steps + averages[[4]] 
    }
  }
  
  new_avg_fw <- new_avg_fw/tot_steps
  new_avg_vert <- new_avg_vert/tot_steps
  new_avg_side <- new_avg_side/tot_steps
  
  return(list(new_avg_fw, new_avg_vert, new_avg_side, tot_steps))
}

#Function which returns a dataframe with all rescaled and cycled information
#Contains data for individual steps, but cycled based on its movement
df_vert_from_mvm <- function(data, n, start_function, filter_function, filter_par){
  # max_steps <- sum(lengths(unlist(data, recursive = F)))
  # vert_df <- data.frame(matrix(ncol = n, nrow = max_steps))
  # colnames(vert_df) <- as.character(1:n)
  
  all_steps <- list()
  
  for(mvm in data){
    mvm_steps <- list()
    
    if(mvm$movement_type == "Walk" & filter_function(mvm, filter_par)){
      
      end_bw <- max(mvm$data$in_step_bw)
      end_fw <- max(mvm$data$in_step_fw)
      
      for(j in 1:end_bw){
        step <- filter(mvm$data, in_step_bw == j)
        
        new_vert <- rescale(step$acc_vert, n)
        
        mvm_steps <- append(mvm_steps, list(new_vert))
      }
      
      for(j in 1:end_fw){
        step <- filter(mvm$data, in_step_fw == j)
        new_vert <- rescale(step$acc_vert, n)
        
        mvm_steps <- append(mvm_steps, list(new_vert))
      }
      
      mvm_vert_avg <- numeric(n)
    }
    
    if(length(mvm_steps) > 0){
      for(j in 1:length(mvm_steps)){
        mvm_vert_avg <- mvm_vert_avg + mvm_steps[[j]]
      }
      start_index <- start_function(mvm_vert_avg)
      #Now we cycle every rescaled step in mvm_steps individually and add them to the df
      for(step in mvm_steps){
        if(start_index != 1){
          step <-  c(step[start_index:n], step[1:(start_index-1)])
          all_steps <- append(all_steps, list(step))
        }
      }
    }
  }
  vert_df <- rbindlist(lapply(all_steps, as.list))
  #vert_df <- as.data.frame(do.call(rbind, all_steps))
  return(vert_df)
}

df_fw_from_mvm <- function(data, n, start_function, filter_function, filter_par){
  # max_steps <- sum(lengths(unlist(data, recursive = F)))
  # fw_df <- data.frame(matrix(ncol = n, nrow = max_steps))
  # colnames(fw_df) <- as.character(1:n)
  
  all_steps <- list()
  
  for(mvm in data){
    mvm_steps <- list()
    
    if(mvm$movement_type == "Walk" & filter_function(mvm, filter_par)){
      
      end_bw <- max(mvm$data$in_step_bw)
      end_fw <- max(mvm$data$in_step_fw)
      
      for(j in 1:end_bw){
        step <- filter(mvm$data, in_step_bw == j)
        new_fw <- rescale(step$acc_fw, n)
        
        mvm_steps <- append(mvm_steps, list(new_fw))
      }
      
      for(j in 1:end_fw){
        step <- filter(mvm$data, in_step_fw == j)
        new_fw <- rescale(step$acc_fw, n)
        
        mvm_steps <- append(mvm_steps, list(new_fw))
      }
      
      mvm_fw_avg <- numeric(n)
    }
    
    if(length(mvm_steps) > 0){
      for(j in 1:length(mvm_steps)){
        mvm_fw_avg <- mvm_fw_avg + mvm_steps[[j]]
      }
      start_index <- start_function(mvm_fw_avg)
      #Now we cycle every rescaled step in mvm_steps individually and add them to the df
      for(step in mvm_steps){
        if(start_index != 1){
          step <-  c(step[start_index:n], step[1:(start_index-1)])
          all_steps <- append(all_steps, list(step))
        }
      }
    }
  }
  fw_df <- rbindlist(lapply(all_steps, as.list))
  #fw_df <- as.data.frame(do.call(rbind, all_steps))
  return(fw_df)
}

df_side_from_mvm <- function(data, n, start_function, filter_function, filter_par){
  # max_steps <- sum(lengths(unlist(data, recursive = F)))
  # side_df <- data.frame(matrix(ncol = n, nrow = max_steps))
  # colnames(side_df) <- as.character(1:n)
  
  all_steps <- list()
  
  for(mvm in data){
    mvm_steps <- list()
    
    if(mvm$movement_type == "Walk" & filter_function(mvm, filter_par)){
      
      end_bw <- max(mvm$data$in_step_bw)
      end_fw <- max(mvm$data$in_step_fw)
      
      for(j in 1:end_bw){
        step <- filter(mvm$data, in_step_bw == j)
        
        new_side <- rescale(step$acc_side, n)
        
        mvm_steps <- append(mvm_steps, list(new_side))
      }
      
      for(j in 1:end_fw){
        step <- filter(mvm$data, in_step_fw == j)
        new_side <- rescale(step$acc_side, n)
        
        mvm_steps <- append(mvm_steps, list(new_side))
      }
      
      mvm_side_avg <- numeric(n)
    }
    
    if(length(mvm_steps) > 0){
      for(j in 1:length(mvm_steps)){
        mvm_side_avg <- mvm_side_avg + mvm_steps[[j]]
      }
      start_index <- start_function(mvm_side_avg)
      #Now we cycle every rescaled step in mvm_steps individually and add them to the df
      for(step in mvm_steps){
        if(start_index != 1){
          step <-  c(step[start_index:n], step[1:(start_index-1)])
          all_steps <- append(all_steps, list(step))
        }
      }
    }
  }
  side_df <- rbindlist(lapply(all_steps, as.list))
  #side_df <- as.data.frame(do.call(rbind, all_steps))
  return(side_df)
}
