Walk1 <- read.gt3x("Data_raw/walk_test_1.gt3x")
Walk1 <- as.data.frame(Walk1)
Walk1$time <- force_tz(Walk1$time, tzone = "Europe/Berlin")

#W1
start_time <- as.POSIXct("2025-01-20 10:31:29.000")
end_time <- as.POSIXct("2025-01-20 10:37:27.000")

#W2
start_time <- as.POSIXct("2025-01-20 10:50:03.000")
end_time <- as.POSIXct("2025-01-20 10:56:05.000")

#W3
start_time <- as.POSIXct("2025-01-20 11:38:13.000")
end_time <- as.POSIXct("2025-01-20 11:44:21.000")


filter(Walk1, time > start_time & time < end_time) %>% ggplot(aes(x = time)) + geom_line(aes(y = Y), colour = "blue") + geom_line(aes(y = Z), colour = "green") + geom_line(aes(y = X), colour = "red")

###Afternoon

Walk2 <- read.gt3x("Data_raw/walk_test_2.gt3x")
Walk2 <- as.data.frame(Walk2)
Walk2$time <- force_tz(Walk2$time, tzone = "Europe/Berlin")

#Zoom in
start_time <- as.POSIXct("2025-01-20 15:43:58.000")
end_time <- as.POSIXct("2025-01-20 15:44:03.000")

filter(Walk2, time > start_time & time < end_time) %>% ggplot(aes(x = time)) + geom_line(aes(y = Y), colour = "blue") + geom_line(aes(y = Z), colour = "green") + geom_line(aes(y = X), colour = "red")

filter(Walk2, time > start_time & time < end_time) %>% pull(Y) %>% na.omit() %>% scale(center = T, scale = F) %>% fft() %>%  Mod() %>% plot(type = "h")


###Applying the rotation
start_time <- as.POSIXct("2025-01-20 10:50:09.000")
end_time <- as.POSIXct("2025-01-20 10:50:10.000")

x_mean <- filter(Walk1, time > start_time & time < end_time) %>% pull(X) %>% mean() 
y_mean <- filter(Walk1, time > start_time & time < end_time) %>% pull(Y) %>% mean() 
z_mean <- filter(Walk1, time > start_time & time < end_time) %>% pull(Z) %>% mean() 

rot <- rotation_from_rest(c(x_mean, y_mean, z_mean))
rot_mat <- rot[[1]]

start_time <- as.POSIXct("2025-01-20 10:50:09.000")
end_time <- as.POSIXct("2025-01-20 10:50:20.000")

test_df <- filter(Walk1, time > start_time & time < end_time)

test_df[, 2:4] <- t(rot_mat %*% t(test_df[, 2:4]))
test_df[3] <- test_df[3] - rot[[2]]  
test_df$norm <- sqrt(rowSums(test_df[, 2:4]^2))

plot(test_df$X, type = "l")

###Second test: 
test_df$vel_Y <- cumsum(test_df$Y)
test_df$vel_Y[60:90] %>% plot()

###Applying rotation to my own walking data (more regular? + offsets incorporated)
start_time <- as.POSIXct("2024-10-10 08:20:09.600")
end_time <- as.POSIXct("2024-10-10 08:20:10.500")

x_mean <- filter(A20_off, time > start_time & time < end_time) %>% pull(X) %>% mean() 
y_mean <- filter(A20_off, time > start_time & time < end_time) %>% pull(Y) %>% mean() 
z_mean <- filter(A20_off, time > start_time & time < end_time) %>% pull(Z) %>% mean() 

rot <- rotation_from_rest(c(x_mean, y_mean, z_mean))
rot_mat <- rot[[1]]

start_time <- as.POSIXct("2024-10-10 08:21:22.900")
end_time <- as.POSIXct("2024-10-10 08:21:24.300")

test_df <- filter(A20_off, time > start_time & time < end_time)

test_df[, 2:4] <- t(rot_mat %*% t(test_df[, 2:4]))
test_df[3] <- test_df[3] - rot[[2]]  
test_df$norm <- sqrt(rowSums(test_df[, 2:4]^2))

plot(test_df$Y, type = "l")
mean(test_df$Y)

###Second test: 
test_df$vel_X <- cumsum(test_df$X)
test_df$vel_Y <- cumsum(test_df$Y)
test_df$vel_Z <- cumsum(test_df$Z)
test_df$vel_Z %>% plot() #Maybe change such that location at end is 0 for Y?
test_df %>% view()
