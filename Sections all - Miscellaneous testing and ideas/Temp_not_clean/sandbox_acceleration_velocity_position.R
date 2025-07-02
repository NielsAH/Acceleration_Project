acc_to_vel <- function(df_col, gravity, starting_vel = 0){
  #Some adjustment? For example, round vel to 0 if very low
  return(cumsum(df_col - gravity) + starting_vel)
}
#Might want to shift by one, but not clear how acceleration measured
#Average over the ms, or acceleration at that one ms

temp <- c(-2,-1.5,-1,-0.5,0,-0.5,-1,-2,-1.1,-1, -0.6)
temp2 <- acc_to_vel(temp, 1)

vel_to_pos <- function(df_col, frequency = 30, starting_pos = 0){
  #Some adjustment? For example, round position change to 0 if vel very low
  return(cumsum(df_col)/frequency + starting_pos)
}

vel_to_pos(temp2)

#Determining rest values for W1
start_time <- as.POSIXct("2025-01-20 10:31:28.000")
end_time <- as.POSIXct("2025-01-20 10:31:30.000")
filter(batten1.3, time > start_time & time < end_time)$Z %>% plot()
x_grav <- filter(batten1.3, time > start_time & time < end_time)$X %>% mean()
y_grav <- filter(batten1.3, time > start_time & time < end_time)$Y %>% mean()
z_grav <- filter(batten1.3, time > start_time & time < end_time)$Z %>% mean()

#W1
start_time <- as.POSIXct("2025-01-20 10:31:28.000")
end_time <- as.POSIXct("2025-01-20 10:31:50.000")
y_col <- filter(batten1.3, time >= start_time & time <= end_time)$Y
y_vel <- y_col %>% acc_to_vel(., gravity = y_grav)
y_pos <- y_vel %>% vel_to_pos()
plot(y_pos[1:1000])

#Testing if norm does behave well
adj_temp <- filter(batten1.3, time > start_time & time < end_time)[,2:4]
adj_temp$X <- adj_temp$X - x_grav
adj_temp$Y <- adj_temp$Y - y_grav
adj_temp$Z <- adj_temp$Z - z_grav

norm <- adj_temp$X^2 + adj_temp$Y^2 + adj_temp$Z^2
plot(norm[300:400], type = "l")
plot(adj_temp$Z[300:400])
