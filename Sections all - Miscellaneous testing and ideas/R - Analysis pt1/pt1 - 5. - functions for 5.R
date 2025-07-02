#First a function that takes a certain metric of one activity
activity_summary <- function(df, idx, col_num, func, ...) {
  
  start_idx <- idx[1]
  stop_idx <- idx[2]
  column <- df[start_idx:stop_idx, col_num]
  return(func(column, ...))
  
}

#Help function that takes the mean of the absolute values, instead of just the mean
mean_abs <- function(x, ...) {mean(abs(x), ...)}


#We now create a function that takes a list (containing activities of a certain type)
#and then takes a certain function of a certain column, such as mean or standard deviation.
#It does so for every element in the list, and then outputs the min, the max and the mean over
#the whole list. This means short activities are weighted as much as long activities, but we
#want this, as on different occasions the positioning of the meter may for example be different.
#We could always decide to input activities or the same length.
#Or we can build an alternative to this function that takes length into account, see below.

activities_summary <- function(df, idx_list, col_num, func, ...) {
  
  metric_list <- c()
  
  for(act in idx_list) {
    start_idx <- act[1]
    stop_idx <- act[2]
    column <- df[start_idx:stop_idx, col_num]
    metric_list <- append(metric_list, func(column, ...))
  }

  return(c(mean(metric_list), min(metric_list), max(metric_list)))

}

#This alternative function will take length into account, by taking the indices of any element in
#the activity list, and just putting them behind each other directly and then computing mean, standard
#deviation etc. from there. If the position of accelerometer is important, this should lead to vastly 
#different results for certain metrics.

#Here function




#Now we add a function that takes a list, and adds all relevant information that can be extracted
#from that list to a dataframe, all for the activity information table

add_activity_information <- function(df, activity_idx, activity_name) {
  
  for(act in activity_idx){
    activity <- activity_name
    activity_length <- (act[2] - act[1] + 1)
    X_mean <- activity_summary(df_part_trim, act, 2, mean, na.rm=T)
    X_sd <- activity_summary(df_part_trim, act, 2, sd, na.rm=T)
    Y_mean <- activity_summary(df_part_trim, act, 3, mean, na.rm=T)
    Y_sd <- activity_summary(df_part_trim, act, 3, sd, na.rm=T)
    Z_mean <- activity_summary(df_part_trim, act, 4, mean, na.rm=T)
    Z_sd <- activity_summary(df_part_trim, act, 4, sd, na.rm=T)
    X_diff_mean <- activity_summary(df_part_trim, act, 10, mean_abs, na.rm=T)
    X_diff_sd <- activity_summary(df_part_trim, act, 10, sd, na.rm=T) 
    Y_diff_mean <- activity_summary(df_part_trim, act, 11, mean_abs, na.rm=T)
    Y_diff_sd <- activity_summary(df_part_trim, act, 11, sd, na.rm=T)
    Z_diff_mean <- activity_summary(df_part_trim, act, 12, mean_abs, na.rm=T)
    Z_diff_sd <-  activity_summary(df_part_trim, act, 12, sd, na.rm=T)
  
    df[(1 + ifelse(is.null(nrow(df)), 0,  nrow(df))), ] <- c(activity, activity_length, X_mean, X_sd, Y_mean, Y_sd, Z_mean, Z_sd, X_diff_mean, X_diff_sd, Y_diff_mean, Y_diff_sd, Z_diff_mean, Z_diff_sd)
  }
  
  return(df)
}
