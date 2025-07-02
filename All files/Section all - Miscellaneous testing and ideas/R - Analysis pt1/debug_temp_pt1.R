trim_activity_partition_3 <- function(df, n_ma, col_val, col_part, threshold){
  n_skip = (n_ma-1)/2
  
  for (part_id in unique(df[[col_part]][df[[col_part]] != 0])) {
    
    indices <- which(df[[col_part]] == part_id)
    start <- min(indices)
    stop <- max(indices)
    
    if((start - 2*n_skip) >= 1 & (start - 1 + 2*n_skip) <= nrow(df)){
      ma_left <- ma_left(df[(start - 2*n_skip):(start - 1 + n_skip), col_val])
      ma_left <- ma_left[!is.na(ma_left)] #Removes the first n_skip values that were just needed for the computation of the relevant values anyway
      
      ma_right <- ma_right(df[(start - n_skip):(start - 1 + 2*n_skip), col_val]) 
      ma_right <- ma_right[!is.na(ma_right)]
    
      new_start <- start - n_skip + which.min(ma_left - ma_right)
    }
    
    if((stop + 2*n_skip) <= nrow(df) & (stop + 1 - 2*n_skip) >= 1){
      ma_left <- ma_left(df[(stop + 1 - 2*n_skip):(stop + n_skip), col_val])
      ma_left <- ma_left[!is.na(ma_left)]
      
      ma_right <- ma_right(df[(stop + 1 - n_skip):(stop + 2*n_skip), col_val]) 
      ma_right <- ma_right[!is.na(ma_right)]
      
      new_stop <- stop - n_skip + which.max(ma_left - ma_right) #Changed stop+1 to stop. With stop+1 it would be start of inactive period
    }
    
    df[indices, col_part] <- 0
    df[new_start:new_stop, col_part] <- part_id 
  }
  return(df)
}


test_array <- c(rep(0, 400), rep(10, 400), rep(0, 400))
test_df <- as.data.frame(test_array)
test_df <- add_ma_to_df(test_df, "ma", 1, 181)

test_df <- activity_partition_to_df(test_df, 2, 181, 1, 1.0)
test_df <- trim_activity_partition_3(test_df, 181, 1, 3, 1.0)

view(test_df)
