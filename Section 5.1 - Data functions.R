add_step_metric_idv <- function(df, new_col_name, filter_function, filter_par, data_function, data_par, metric_function, metric_par){

  assign(new_col_name, c()) 
  
  for(idv in df$individual){
    
    steps_data <- c()
    sequence_lists <- get(paste0(idv, "_list"))
    
    for(seq_list in sequence_lists){
      
      for(seq in seq_list){
        #If I do want to generalise even more: would be nicer
        #check <- do.call(filter_function, seq, args[names(args) %in% names(formals(filter_function))])
        if(seq$movement_type == "Walk" & filter_function(seq, filter_par)){
          
          end_bw <- max(seq$data$in_step_bw)
          end_fw <- max(seq$data$in_step_fw)
          
          
          if(end_bw > 1){
            for(j in 1:(end_bw - 1)){
              
              step <- filter(seq$data, in_step_bw == j)
              data <- data_function(step, data_par)
              steps_data <- append(steps_data, data)
            }
          }
          
          if(end_fw > 1){
            for(j in 1:(end_fw - 1)){
              step <- filter(seq$data, in_step_fw == j)
              data <- data_function(step, data_par)
              steps_data <- append(steps_data, data)
            }
          }
          
        }
      }
    }
    
    if(length(steps_data) == 0){
      assign(new_col_name, append(get(new_col_name), NA))
    }
    else{
      new_val <- metric_function(steps_data, metric_par) 
      assign(new_col_name, append(get(new_col_name), new_val))
    }
  }
  
  df[[new_col_name]] <- get(new_col_name)
  return(df)
   
}

add_seq_metric_idv <- function(df, new_col_name, filter_function, filter_par, data_function, data_par, metric_function, metric_par){

  assign(new_col_name, c()) 
  
  for(idv in df$individual){
    
    seq_data <- c()
    sequence_lists <- get(paste0(idv, "_list"))
    
    for(seq_list in sequence_lists){
      
      for(seq in seq_list){
        if(seq$movement_type == "Walk" & filter_function(seq, filter_par)){
          
          data <- data_function(seq, data_par)
          seq_data <- append(seq_data, data)
          
        }
      }
    }
    
    if(length(seq_data) == 0){
      assign(new_col_name, append(get(new_col_name), NA))
    }
    else{
      new_val <- metric_function(seq_data, metric_par) 
      assign(new_col_name, append(get(new_col_name), new_val))
    }
  }
  
  df[[new_col_name]] <- get(new_col_name)
  return(df)
}

add_step_metric_day <- function(df, new_col_name, filter_function, filter_par, data_function, data_par, metric_function, metric_par){
  
  assign(new_col_name, c()) 
  
  for(day in df$day){
    
    steps_data <- c()
    
    seq_list <- get(paste0("seq_", day))
      
      for(seq in seq_list){
        if(seq$movement_type == "Walk" & filter_function(seq, filter_par)){
          
          end_bw <- max(seq$data$in_step_bw)
          end_fw <- max(seq$data$in_step_fw)
          
          
          if(end_bw > 1){
            for(j in 1:(end_bw - 1)){
              
              step <- filter(seq$data, in_step_bw == j)
              data <- data_function(step, data_par)
              steps_data <- append(steps_data, data)
            }
          }
          
          if(end_fw > 1){
            for(j in 1:(end_fw - 1)){
              step <- filter(seq$data, in_step_fw == j)
              data <- data_function(step, data_par)
              steps_data <- append(steps_data, data)
            }
          }
          
        }
      }
    
    if(length(steps_data) == 0){
      assign(new_col_name, append(get(new_col_name), NA))
    }
    else{
      new_val <- metric_function(steps_data, metric_par) 
      assign(new_col_name, append(get(new_col_name), new_val))
    }
  }
  
  df[[new_col_name]] <- get(new_col_name)
  return(df)
  
}


add_seq_metric_day <- function(df, new_col_name, filter_function, filter_par, data_function, data_par, metric_function, metric_par){
  
  assign(new_col_name, c()) 
  
  for(day in df$day){
    
    seq_data <- c()
    
    seq_list <- get(paste0("seq_", day))
    
    for(seq in seq_list){
      if(seq$movement_type == "Walk" & filter_function(seq, filter_par)){
        
       data <- data_function(seq, data_par)
       seq_data <- append(seq_data, data)
       
      }
    }
    
    if(length(seq_data) == 0){
      assign(new_col_name, append(get(new_col_name), NA))
    }
    else{
      new_val <- metric_function(seq_data, metric_par) 
      assign(new_col_name, append(get(new_col_name), new_val))
    }
  }
  
  df[[new_col_name]] <- get(new_col_name)
  return(df)
  
}
