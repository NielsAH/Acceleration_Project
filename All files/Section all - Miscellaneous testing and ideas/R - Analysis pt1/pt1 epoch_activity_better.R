#Check running and check gym. Compare with graphs using vec_diff
start_time <- as.POSIXct("2024-10-10 08:25.000")
end_time <- as.POSIXct("2024-10-10 08:29.000")

epoch_1 %>% filter(TimeStamp >= start_time & TimeStamp <= end_time) %>% 
  ggplot(aes(x = TimeStamp)) + 
  geom_line(aes(y = sqrt(axis1^2 + axis2^2 + axis3^2)))

temp_tot <- df$vec_diff
diff_x <- df$X_diff
diff_z <- df$Z_diff
X <- df$X
tot_raw = tapply(abs(temp_tot), (seq_along(temp_tot) - 1) %/% 30, mean) 
X_raw = tapply(abs(diff_x), (seq_along(diff_x) - 1) %/% 30, mean)
X_mean = tapply(abs(X), (seq_along(diff_x) - 1) %/% 30, mean)
Z_raw = tapply(abs(diff_z), (seq_along(diff_z) - 1) %/% 30, mean) 
epoch_1_temp <- mutate(epoch_1, alt_act = tot_raw)
epoch_1_temp <- mutate(epoch_1_temp, X_act = X_raw)
epoch_1_temp <- mutate(epoch_1_temp, X_mean = X_mean)
epoch_1_temp <- mutate(epoch_1_temp, Z_act = Z_raw)

epoch_1_temp %>% filter(TimeStamp >= start_time & TimeStamp < end_time) %>% 
  ggplot(aes(x = TimeStamp)) + 
  geom_line(aes(y = Z_act)) + 
  geom_line(aes(y = axis3/100), colour = "red")

epoch_1_temp %>% filter(TimeStamp >= start_time & TimeStamp < end_time) %>% 
  ggplot(aes(x = TimeStamp)) + 
  geom_line(aes(y = X_act)) + 
  geom_line(aes(y = axis1/800), colour = "red") 



k = 527109
#mean(sqrt(df$X_diff[(k*30 - 29):(k*30)]^2 + df$Y_diff[(k*30 - 29):(k*30)]^2 + df$Z_diff[(k*30 - 29):(k*30)]^2))
#epoch_1_temp$alt_act[k]

mean(sqrt(df$X_diff[(k*30 - 29):(k*30)]^2 + df$Y_diff[(k*30 - 29):(k*30)]^2 + df$Z_diff[(k*30 - 29):(k*30)]^2))
mean(abs(df$X_diff[(k*30 - 29):(k*30)]))
mean(abs(df$Y_diff[(k*30 - 29):(k*30)]))
mean(abs(df$Z_diff[(k*30 - 29):(k*30)]))


#Same analysis at a different time
start_time <- as.POSIXct("2024-10-12 17:21:10.000")
end_time <- as.POSIXct("2024-10-12 17:21:51.000")

epoch_1_temp %>% filter(TimeStamp >= start_time & TimeStamp < end_time) %>% 
  ggplot(aes(x = TimeStamp)) + 
  geom_line(aes(y = Z_act))

epoch_1 %>% filter(TimeStamp >= start_time & TimeStamp < end_time) %>% 
  ggplot(aes(x = TimeStamp)) + 
  geom_line(aes(y = axis3))

df %>% filter(time >= start_time & time < end_time) %>% 
  ggplot(aes(x = time)) + 
  geom_line(aes(y = Z))

df %>% filter(time >= start_time & time < end_time) %>% 
  ggplot(aes(x = time)) + 
  geom_line(aes(y = Z_diff))

