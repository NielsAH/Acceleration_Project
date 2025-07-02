act_thresh <- 10
inact_thresh <- 10

count <- 0
act_count <- numeric(length(sat1_pt1_df1$vel_tot))

for (i in 1:length(sat1_pt1_df1$vel_tot)) {
  if (sat1_pt1_df1$vel_tot[i] != 0) {
    count <- count + 1  
    act_count[i] <- count  
  } else {
    count <- 0 
    act_count[i] <- 0  
  }
}

count <- 0
inact_count <- numeric(length(sat1_pt1_df1$vel_tot))

for (i in 1:length(sat1_pt1_df1$vel_tot)) {
  if (sat1_pt1_df1$vel_tot[i] == 0) {
    count <- count + 1  
    inact_count[i] <- count  
  } else {
    count <- 0 
    inact_count[i] <- 0  
  }
}
sat1_pt1_df3 <- sat1_pt1_df1

# sat1_pt1_df3 <- sat1_pt1_df1 %>%
#   mutate(act_count = act_count) %>%
#   mutate(inact_count = inact_count)


act_starts_2 <- numeric(length(act_count))
act_stops_2 <- numeric(length(act_count))

is_active <- FALSE
is_inactive <- TRUE

for (i in seq_along(act_count)) {
  if (!is_active && act_count[i] >= act_thresh) {
    act_starts_2[i] <- 1  
    is_active <- TRUE      
    is_inactive <- FALSE   
  }
  if (!is_inactive && inact_count[i] >= inact_thresh) {
    act_stops_2[i] <- 1 # Mark the start of an inactive period
    is_active <- FALSE      # Reset active flag
    is_inactive <- TRUE     # Set inactive flag to TRUE
  }
}

activity_starts_2 <- which(act_starts_2 != 0)
activity_stops_2 <- which(act_stops_2 != 0)

if(length(activity_stops_2) < length(activity_starts_2)){activity_stops_2 <- c(activity_stops_2, length(sat1_pt1_df3$vel_tot))}
#Note in this case the values where ma is NA at the end are also included!

activity_labels_2 <- rep(0, length(sat1_pt1_df3$vel_tot))

length(activity_stops_2)

count = 1
for (i in 1:length(activity_starts_2)) {
  activity_labels_2[(activity_starts_2[i]+1-act_thresh):(activity_stops_2[i]-inact_thresh)] <- count
  count <- count + 1
  
}

sat1_pt1 <- sat1_pt1_df3 %>%
  mutate(part = activity_labels_2)

#May want to filter to only include minimally some number of observations
view(sat1_pt1)