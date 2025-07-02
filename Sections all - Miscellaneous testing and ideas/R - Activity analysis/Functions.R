add_ma_act_part <- function(df, column, ma_param, act_param) {
  
  df <- df %>% 
    mutate(active = ifelse(column != 0, 1, 0)) %>%
    mutate(ma_active = ma(active, n = ma_param))
  
  activity_starts <- which(df$ma_active[2:length(df$ma_active)] >= act_param & (df$ma_active[1:(length(df$ma_active)-1)] < act_param | is.na(df$ma_active[1:(length(df$ma_active)-1)])))+1
  #There can also be activity at the start. First few ma values will also be MA, thus this will recognise that >= 0.2 and NA before also is the start of activity. Although the values before that where the ma is NA should then be taken into account as well!
  
  activity_stops <- which(df$ma_active[2:length(df$ma_active)] <= act_param & df$ma_active[1:(length(df$ma_active)-1)] > act_param)
  
  if(length(activity_stops) < length(activity_starts)){activity_stops <- c(activity_stops, length(df$ma_active))}
  #Note in this case the values where ma is NA at the end are also included!
  
  activity_labels <- rep(0, length(df$ma_active))
  
  count = 1
  
  for (i in 1:length(activity_starts)) {
    if(length(activity_starts[i]:activity_stops[i]) >= ma_param) {
      activity_labels[activity_starts[i]:activity_stops[i]] <- count
      count <- count + 1
    }
  }
  
  
  df <- df %>%
    mutate(part = activity_labels)
  
  return(df)
}


add_hard_act_part <- function(df, column, act_thresh, inact_thresh) {
  
  count <- 0
  act_count <- numeric(length(column))
  
  for (i in 1:length(column)) {
    if (column[i] != 0) {
      count <- count + 1  
      act_count[i] <- count  
    } else {
      count <- 0 
      act_count[i] <- 0  
    }
  }
  
  count <- 0
  inact_count <- numeric(length(column))
  
  for (i in 1:length(column)) {
    if (column[i] == 0) {
      count <- count + 1  
      inact_count[i] <- count  
    } else {
      count <- 0 
      inact_count[i] <- 0  
    }
  }

  activity_starts <- numeric(length(act_count))
  activity_stops <- numeric(length(act_count))
  
  is_active <- FALSE
  is_inactive <- TRUE
  
  for (i in seq_along(act_count)) {
    if (!is_active && act_count[i] >= act_thresh) {
      activity_starts[i] <- 1  
      is_active <- TRUE      
      is_inactive <- FALSE   
    }
    if (!is_inactive && inact_count[i] >= inact_thresh) {
      activity_stops[i] <- 1 # Mark the start of an inactive period
      is_active <- FALSE      # Reset active flag
      is_inactive <- TRUE     # Set inactive flag to TRUE
    }
  }
  
  activity_starts <- which(activity_starts != 0)
  activity_stops <- which(activity_stops != 0)
  
  if(length(activity_stops) < length(activity_starts)){activity_stops <- c(activity_stops, length(column))}
  #Note in this case the values where ma is NA at the end are also included!
  
  activity_labels <- rep(0, length(column))
  
  count = 1
  for (i in 1:length(activity_starts)) {
    activity_labels[(activity_starts[i]+1-act_thresh):(activity_stops[i]-inact_thresh)] <- count
    count <- count + 1
  }
  
  df <- df %>%
    mutate(part = activity_labels)
  
  return(df)
  
}

