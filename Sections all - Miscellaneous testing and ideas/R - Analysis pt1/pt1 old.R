### Code for looping over and changing all values in the dataframe

### Below code also too slow
df[(start_index):(stop_index), "incline_off"] <- incline_off
df[(start_index):(stop_index), "incline_sit"] <- incline_sit
df[(start_index):(stop_index), "incline_stand"] <- incline_stand
df[(start_index):(stop_index), "incline_lie"] <- incline_lie
df[(start_index):(stop_index), "steps"] <- steps
df[(start_index):(stop_index), "worn"] <- worn

###Below code that is an alternative, but too slow to recalculate everything
df[(1 + 30*(j-1)):(30*j), "incline_off"] <- incline_off
df[(1 + 30*(j-1)):(30*j), "incline_sit"] <- incline_sit
df[(1 + 30*(j-1)):(30*j), "incline_stand"] <- incline_stand
df[(1 + 30*(j-1)):(30*j), "incline_lie"] <- incline_lie
df[(1 + 30*(j-1)):(30*j), "steps"] <- steps
df[(1 + 30*(j-1)):(30*j), "worn"] <- worn

###Below code is an alternative, but seems a bit slower. Maybe implement later
  for(attribute in c("incline_off", "incline_sit", "incline_stand", "incline_lie", "steps", "worn")){
  df[(1 + 30*(j-1)):(30*j), attribute] <- get(attribute)
}

###Below code would also work, just way too slow
df[df$time >= start_time & df$time < end_time, "incline_off"] <- incline_off
df[df$time >= start_time & df$time < end_time, "incline_sit"] <- incline_sit
df[df$time >= start_time & df$time < end_time, "incline_stand"] <- incline_stand
df[df$time >= start_time & df$time < end_time, "incline_lie"] <- incline_lie
df[df$time >= start_time & df$time < end_time, "steps"] <- steps
df[df$time >= start_time & df$time < end_time, "worn"] <- worn

#Other old code to do the same

view(df[1:500000,])

incline_off_list <- list()
incline_stand_list <- list()
incline_sit_list <- list()
incline_lie_list <- list()
steps_list <- list()

for(j in 1:nrow(epoch_1[1:10000,])){
  
  #Using numbers makes it way quicker, although a bit harder to generalise in a function
  steps <- epoch_1[j,5]
  incline_off <- epoch_1[j, 7]
  incline_stand <- epoch_1[j,8]
  incline_sit <- epoch_1[j,9]
  incline_lie <- epoch_1[j,10]
  
  incline_off_list[[j]] <- rep(incline_off, 30)
  incline_stand_list[[j]] <- rep(incline_stand, 30)
  incline_sit_list[[j]] <- rep(incline_sit, 30)
  incline_lie_list[[j]] <- rep(incline_lie, 30)
  steps_list[[j]] <- rep(steps, 30)
}

df$incline_off <- unlist(incline_off_list)
df$incline_sit <- unlist(incline_sit_list)
df$incline_stand <- unlist(incline_stand_list)
df$incline_lie <- unlist(incline_lie_list)
df$steps <- unlist(steps_list)
df$worn <- unlist(worn_list)

view(df[1:500000,])

df[df$time >= start_time & df$time < end_time, "incline_off"] <- incline_off
view(df[df$time >= start_time & df$time < end_time, "incline_off"])

### For classification

### Mean and standard deviation

We first see if and how much the different activities differ in mean and standard deviation on the three axes.

Walking:
  
  ```{r}
activity_summary(df_part_trim, walk_idx, 2, mean, na.rm = T)
activity_summary(df_part_trim, walk_idx, 2, sd, na.rm = T)

activity_summary(df_part_trim, walk_idx, 3, mean, na.rm = T)
activity_summary(df_part_trim, walk_idx, 3, sd, na.rm = T)

activity_summary(df_part_trim, walk_idx, 4, mean, na.rm = T)
activity_summary(df_part_trim, walk_idx, 4, sd, na.rm = T)

activity_summary(df_part_trim, walk_idx, 10, function(x, ...) {(mean(abs(x), ...))
}, na.rm = T)
activity_summary(df_part_trim, walk_idx, 10, sd, na.rm = T)

activity_summary(df_part_trim, walk_idx, 11, function(x, ...) {(mean(abs(x), ...))
}, na.rm = T)
activity_summary(df_part_trim, walk_idx, 11, sd, na.rm = T)

activity_summary(df_part_trim, walk_idx, 12, function(x, ...) {(mean(abs(x), ...))
}, na.rm = T)
activity_summary(df_part_trim, walk_idx, 12, sd, na.rm = T)
```
Cycling:
  ```{r}
activity_summary(df_part_trim, cycle_idx, 2, mean, na.rm = T)
activity_summary(df_part_trim, cycle_idx, 2, sd, na.rm = T)

activity_summary(df_part_trim, cycle_idx, 3, mean, na.rm = T)
activity_summary(df_part_trim, cycle_idx, 3, sd, na.rm = T)

activity_summary(df_part_trim, cycle_idx, 4, mean, na.rm = T)
activity_summary(df_part_trim, cycle_idx, 4, sd, na.rm = T)

activity_summary(df_part_trim, cycle_idx, 10, mean, na.rm = T)
activity_summary(df_part_trim, cycle_idx, 10, sd, na.rm = T)

activity_summary(df_part_trim, cycle_idx, 11, mean, na.rm = T)
activity_summary(df_part_trim, cycle_idx, 11, sd, na.rm = T)

activity_summary(df_part_trim, cycle_idx, 12, mean, na.rm = T)
activity_summary(df_part_trim, cycle_idx, 12, sd, na.rm = T)
```

Running:
  ```{r}
activity_summary(df_part_trim, run_idx, 2, mean, na.rm = T)
activity_summary(df_part_trim, run_idx, 2, sd, na.rm = T)

activity_summary(df_part_trim, run_idx, 3, mean, na.rm = T)
activity_summary(df_part_trim, run_idx, 3, sd, na.rm = T)

activity_summary(df_part_trim, run_idx, 4, mean, na.rm = T)
activity_summary(df_part_trim, run_idx, 4, sd, na.rm = T)

activity_summary(df_part_trim, run_idx, 10, mean, na.rm = T)
activity_summary(df_part_trim, run_idx, 10, sd, na.rm = T)

activity_summary(df_part_trim, run_idx, 11, mean, na.rm = T)
activity_summary(df_part_trim, run_idx, 11, sd, na.rm = T)

activity_summary(df_part_trim, run_idx, 12, mean, na.rm = T)
activity_summary(df_part_trim, run_idx, 12, sd, na.rm = T)
```

Going up stairs:
  ```{r}
activity_summary(df_part_trim, stairs_up_idx, 2, mean, na.rm = T)
activity_summary(df_part_trim, stairs_up_idx, 2, sd, na.rm = T)

activity_summary(df_part_trim, stairs_up_idx, 3, mean, na.rm = T)
activity_summary(df_part_trim, stairs_up_idx, 3, sd, na.rm = T)

activity_summary(df_part_trim, stairs_up_idx, 4, mean, na.rm = T)
activity_summary(df_part_trim, stairs_up_idx, 4, sd, na.rm = T)

activity_summary(df_part_trim, stairs_up_idx, 10, mean, na.rm = T)
activity_summary(df_part_trim, stairs_up_idx, 10, sd, na.rm = T)

activity_summary(df_part_trim, stairs_up_idx, 11, mean, na.rm = T)
activity_summary(df_part_trim, stairs_up_idx, 11, sd, na.rm = T)

activity_summary(df_part_trim, stairs_up_idx, 12, mean, na.rm = T)
activity_summary(df_part_trim, stairs_up_idx, 12, sd, na.rm = T)
```

Going down stairs:
  ```{r}
activity_summary(df_part_trim, stairs_down_idx, 2, mean, na.rm = T)
activity_summary(df_part_trim, stairs_down_idx, 2, sd, na.rm = T)

activity_summary(df_part_trim, stairs_down_idx, 3, mean, na.rm = T)
activity_summary(df_part_trim, stairs_down_idx, 3, sd, na.rm = T)

activity_summary(df_part_trim, stairs_down_idx, 4, mean, na.rm = T)
activity_summary(df_part_trim, stairs_down_idx, 4, sd, na.rm = T)

activity_summary(df_part_trim, stairs_down_idx, 10, mean, na.rm = T)
activity_summary(df_part_trim, stairs_down_idx, 10, sd, na.rm = T)

activity_summary(df_part_trim, stairs_down_idx, 11, mean, na.rm = T)
activity_summary(df_part_trim, stairs_down_idx, 11, sd, na.rm = T)

activity_summary(df_part_trim, stairs_down_idx, 12, mean, na.rm = T)
activity_summary(df_part_trim, stairs_down_idx, 12, sd, na.rm = T)
```

General standing activity:
  ```{r}
activity_summary(df_part_trim, stand_active_idx, 2, mean, na.rm = T)
activity_summary(df_part_trim, stand_active_idx, 2, sd, na.rm = T)

activity_summary(df_part_trim, stand_active_idx, 3, mean, na.rm = T)
activity_summary(df_part_trim, stand_active_idx, 3, sd, na.rm = T)

activity_summary(df_part_trim, stand_active_idx, 4, mean, na.rm = T)
activity_summary(df_part_trim, stand_active_idx, 4, sd, na.rm = T)

activity_summary(df_part_trim, stand_active_idx, 10, mean, na.rm = T)
activity_summary(df_part_trim, stand_active_idx, 10, sd, na.rm = T)

activity_summary(df_part_trim, stand_active_idx, 11, mean, na.rm = T)
activity_summary(df_part_trim, stand_active_idx, 11, sd, na.rm = T)

activity_summary(df_part_trim, stand_active_idx, 12, mean, na.rm = T)
activity_summary(df_part_trim, stand_active_idx, 12, sd, na.rm = T)
```

