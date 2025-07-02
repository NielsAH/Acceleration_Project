Walk1 <- read.gt3x("Data_raw/walk_test_1.gt3x")
Walk1 <- as.data.frame(Walk1)
Walk1$time <- force_tz(Walk1$time, tzone = "Europe/Berlin")

###1. Continuation with what we were doing
pnt <- 290000
wdw_size <- 150
ma_data <- 5
ma_energy <- 9
sigma <- 1

temp <- Walk1[(pnt - wdw_size):(pnt+wdw_size),]

rest_vec <- c(mean(temp$X), mean(temp$Y), mean(temp$Z))
rest_x <- mean(temp$X)
rest_y <- mean(temp$Y)
rest_z <- mean(temp$Z)

rot_mat <- rotation_initial(c(rest_x, rest_y, rest_z))[[1]]
temp[, 2:4] <- t(rot_mat %*% t(temp[, 2:4]))

spec <- temp$Y %>% energy_kernel(., ma_energy, ma_gauss, sigma)
#spec_norm <- (spec)/(var(temp$Y))

#Determining first frequency
hill_info <- capture_hills(spec, 2)
first_hill <- c(hill_info[[2]][[1]], hill_info[[3]][[1]], hill_info[[4]][[1]])
second_hill <- c(hill_info[[2]][[2]], hill_info[[3]][[2]], hill_info[[4]][[2]])

###2. Trying to generalise: run the big window over the (middle of) a large dataframe

#W2 3800-3900, 6800-6900, 8800-8900 etc.
start_time <- as.POSIXct("2025-01-20 10:49:30.000")
end_time <- as.POSIXct("2025-01-20 10:56:30.000")
#W3 
start_time <- as.POSIXct("2025-01-20 11:38:00.000")
end_time <- as.POSIXct("2025-01-20 11:45:00.000")
#W1
start_time <- as.POSIXct("2025-01-20 10:31:00.000")
end_time <- as.POSIXct("2025-01-20 10:38:00.000")

df_temp <- filter(Walk1, time > start_time & time < end_time) #Define df

#Example of myself: 2000-2200 good example for left and right foot and different periodicities
start_time <- as.POSIXct("2024-10-10 14:20:00.000")
end_time <- as.POSIXct("2024-10-10 14:26:00.000")

df_temp <- filter(A20_off, time > start_time & time < end_time)

#Example of myself when running
start_time <- as.POSIXct("2024-11-24 07:45:00.000")
end_time <- as.POSIXct("2024-11-24 07:55:00.000")

df_temp <- filter(A21_off, time > start_time & time < end_time)

#Example of Martin
start_time <- as.POSIXct("2024-10-10 14:20:00.000")
end_time <- as.POSIXct("2024-10-10 14:26:00.000")

df_temp <- filter(A20_off, time > start_time & time < end_time)


#Now actually applying
frequencies <- numeric(nrow(df_temp))
relative_frac <- numeric(nrow(df_temp))
absolute_frac <- numeric(nrow(df_temp))
frequencies_2 <- numeric(nrow(df_temp))
relative_frac_2 <- numeric(nrow(df_temp))
absolute_frac_2 <- numeric(nrow(df_temp))

acc_vert <- numeric(nrow(df_temp)) #Rotated Y
acc_hor_X <- numeric(nrow(df_temp)) #Rotated X
acc_hor_Z <- numeric(nrow(df_temp)) #Rotated Z

wdw_size <- 150
ma_data <- 5
ma_energy <- 9
sigma <- 1

for(k in 1:nrow(df_temp)){
  print(k)
  start <- max(1, k - wdw_size)
  end <- min(nrow(df_temp), k + wdw_size)
  temp <- df_temp[(start):(end),]
  
  rest_vec <- c(mean(temp$X), mean(temp$Y), mean(temp$Z))
  rest_x <- mean(temp$X)
  rest_y <- mean(temp$Y)
  rest_z <- mean(temp$Z)
  
  rot_mat <- rotation_initial(c(rest_x, rest_y, rest_z))[[1]]
  temp[, 2:4] <- t(rot_mat %*% t(temp[, 2:4]))
  
  rot_Y <- t(rot_mat %*% t(df_temp[k, 2:4]))[2] #y coordinate in that point
  rot_X <- t(rot_mat %*% t(df_temp[k, 2:4]))[1] #horizontal one direction
  rot_Z <- t(rot_mat %*% t(df_temp[k, 2:4]))[3] #horizontal orthogonal direction
  
  acc_vert[k] <- rot_Y - rest_y #For easier interpretation as well
  acc_hor_X[k] <- rot_X
  acc_hor_Z[k] <- rot_Z
  
  spec <- temp$Y %>% energy_kernel(., ma_energy, ma_gauss, sigma)
  #spec_norm <- (spec)/(var(temp$Y))

  hill_info <- capture_hills(spec, 2)
  frequencies[k] <- hill_info[[2]][[1]]
  relative_frac[k] <- hill_info[[3]][[1]]
  absolute_frac[k] <- hill_info[[4]][[1]]
  frequencies_2[k] <- hill_info[[2]][[2]]
  relative_frac_2[k] <- hill_info[[3]][[2]]
  absolute_frac_2[k] <- hill_info[[4]][[2]]
}

df_temp$ma_Y <- ma_trunc(df_temp$Y, n = ma_data)

df_temp$acc_vert <- acc_vert
df_temp$ma_vert <-  ma_trunc(df_temp$acc_vert, n = ma_data)

df_temp$acc_hor_X <- acc_hor_X
df_temp$acc_hor_Z <- acc_hor_Z
df_temp$acc_hor <- sqrt(acc_hor_X^2 + acc_hor_Z^2)
df_temp$ma_hor <- ma_trunc(df_temp$acc_hor, n = ma_data)

df_temp$row <- seq(1:(nrow(df_temp)))
df_temp$frequencies <- frequencies
df_temp$relative_frac <- relative_frac
df_temp$absolute_frac <- absolute_frac
df_temp$frequencies_2 <- frequencies_2
df_temp$relative_frac_2 <- relative_frac_2
df_temp$absolute_frac_2 <- absolute_frac_2

#The whole determination of frequencies is finished.
#Now we compute the potential peaks
peak_thresh <- 0.1 #Depends on person 
valley_thresh <- -0.15 #Depends on person

df_temp <- df_temp %>%
  mutate(
    step_valley = ifelse(ma_vert < lag(ma_vert, default = Inf) & 
                           ma_vert < lead(ma_vert, default = Inf) & 
                           ma_vert <= valley_thresh, 1, 0)
  )

df_temp <- df_temp %>%
  mutate(
    step_peak = ifelse(ma_vert > lag(ma_vert, default = -Inf) & 
                         ma_vert > lead(ma_vert, default = -Inf) & 
                         ma_vert >= peak_thresh , 1, 0)
  )

df_temp %>% ggplot(aes(x = time, y = frequencies, colour = relative_frac)) + 
  geom_line() +
  scale_colour_viridis_c(option = "plasma") +
  theme_minimal()
df_temp[11000:12000,] %>% ggplot(aes(x = time, y = frequencies, colour = relative_frac)) + 
  geom_line() +
  scale_colour_viridis_c(option = "plasma") +
  theme_minimal()

#Now the hard part starts: identify full sequences of movement
thresh_abs <- 0.01 #Depends heavily on the person. Update based on length etc.
thresh_rel <- 0.15 #Depends on person and precise movement, but less clear in what way
period_len <- 8 #How lenient (or length) in determining where next peak is. Note that we
#require peaks to have some minimum acceleration size, so it is safe to take
#this value quite high: we will notice once activity stops altogether
period_thresh <- 0.15 #How similar steps must be to the last to be included
double_step_thresh <- 0.25 #How similar initial two steps must be
min_step_period <- 5
max_step_period <- 50

in_seq <- numeric(nrow(df_temp)) #All the values already in a seq (and in which)
in_step_fw <- numeric(nrow(df_temp)) #All the values already in a step forward from central (and in which)
in_step_bw <- numeric(nrow(df_temp)) #All the values already in a step backward from central (and in which)

ordering <- order(df_temp$relative_frac, decreasing = TRUE)
sequences <- list()
seq_num <- 0 #Current sequence we are adding

for(j in ordering){
  print(j)
  if(
     (df_temp$step_valley[j] == 1) &
     (df_temp$absolute_frac[j] > thresh_abs) &
     (df_temp$relative_frac[j] > thresh_rel) &
     (df_temp$frequencies[j]) > min_step_period &
     (df_temp$frequencies[j]) < max_step_period &
     (in_seq[j] == 0))
     {
    print(j)
    #Trying to detect and store a sequence
    #Then add everything in this sequence to "completed", also ensure we do not add
    #values that are already in some other sequence! Should not happen, as a 'gap'
    #at start and end should look the same
    
    #First check what the frequency we look at is: either main frequency, or twice main 
    #frequency if twice main frequency is almost precisely a multiple of second frequency
    #and second frequency is sufficient fraction
    freq <- round(df_temp$frequencies[j]*2) #For now we keep it simple. In reality may not want to multiply by 2
    
    rounding <- ifelse(((freq - floor(df_temp$frequencies[j]*2)) == 0), 1, -1)
    #Has value 1 if it was rounded down, e.g. 33.2 to 33. Has value -1 if it was rounded up
    
    
    #We now want to check if we have two nice steps to begin with, else we try looking for a point where we do
    
    if((j + freq + 1 <= nrow(df_temp)) & (j - freq - 1 >= 1)){
      if(sum(in_seq[(j - freq - 1) : (j + freq + 1)]) == 0 ) { #We need the values from the double step not yet in a sequence
      fw <- 0 #How far fw peak is. If stays 0: no peak
      bw <- 0 #How far bw peak is. If stays 0: no peak
      if(df_temp$step_valley[j + freq] == 1){
        fw <- freq
      }
      else if(df_temp$step_valley[j + freq + rounding] == 1){
        fw <- freq + rounding
      }
      else if(df_temp$step_valley[j + freq - rounding] == 1){
        fw <- freq - rounding
      }
      if(df_temp$step_valley[j - freq] == 1){
        bw <- freq
      }
      else if(df_temp$step_valley[j - (freq + rounding)] == 1){
        bw <- freq + rounding
      }
      else if(df_temp$step_valley[j - (freq - rounding)] == 1){
        bw <- freq - rounding
      }
      
      if((bw != 0) & (fw != 0)){ #We need a step forward and a step backward
        double_step <- df_temp$ma_vert[(j - bw : j + fw)]
        acf_at_freq <- acf(double_step, lag.max = freq)$acf[freq+1]
        if(acf_at_freq > double_step_thresh) 
        {
          seq_num <- seq_num + 1 #We really do have a new sequence, as we have at least two steps
          sequences[[seq_num]] <- list(steps_fw = list(), steps_bw = list(), cor_bw = c(), cor_fw = c()) #Correlation with last one (either going forward or backward)
          step_num_fw <- 1
          step_num_bw <- 1 
          
          sequences[[seq_num]]$steps_fw[[step_num_fw]] <- c((j):(j + fw - 1))
          sequences[[seq_num]]$steps_bw[[step_num_bw]] <- c((j - bw):(j - 1))
          sequences[[seq_num]]$cor_fw <- append(sequences[[seq_num]]$cor_fw, acf_at_freq)
          sequences[[seq_num]]$cor_bw <- append(sequences[[seq_num]]$cor_bw, acf_at_freq)
          
          in_seq[(j):(j + fw - 1)] <- seq_num
          in_seq[c((j - bw):(j - 1))] <- seq_num
          in_step_fw[(j):(j + fw - 1)] <- step_num_fw
          in_step_bw[c((j - bw):(j - 1))] <- step_num_bw
          
          #From here go backward and forward to find more steps by checking if similar to last
          #First add steps to the sequence going forward
          
          forward <- T #Still chance of adding forward steps
          end_old <- j
          end_new <- j + fw #Current last one: we already know it is a valley
          
          while(forward){
            peak_found <- F
            
            if((end_new + freq + period_len) > nrow(df_temp)){


              forward <- F #We are at or close to end of dataframe. Cannot add new ones

            }
            else if(sum(in_seq[end_new:(end_new+freq+period_len)]) != 0){

              forward <- F 

              #We do not want to overlap with another sequence of movements. Later can add that we may want
              #to merge here // throw a message, but in principle indicates a break at some point,
              #as otherwise we would have detected this one earlier
            }
            else if(df_temp$step_valley[end_new + freq] == 1){
              #If enough periodicity: add step going forward
              
              acf_at_freq <- acf(df_temp$ma_vert[end_old:(end_new + freq)], lag.max = freq)$acf[freq + 1]
              
              if(acf_at_freq > period_thresh){
                step_num_fw <- step_num_fw + 1
                
                sequences[[seq_num]]$steps_fw[[step_num_fw]] <- c((end_new):(end_new+freq - 1))
                sequences[[seq_num]]$cor_fw <- append(sequences[[seq_num]]$cor_fw, acf_at_freq)
                
                in_seq[(end_new):(end_new+freq - 1)] <- seq_num
                in_step_fw[(end_new):(end_new+freq - 1)] <- step_num_fw
                
                peak_found <- T
                
                end_old <- end_new
                end_new <- end_new + freq
                
              } else{  #If little periodicity/correlation: stop collecting new steps 

                forward <- F
                
              }
            } else{ #If not at lag difference of 0, see if at any other lag
              for(k in 1:period_len){
                if (df_temp$step_valley[end_new + freq + k*rounding] == 1){
                  #If enough periodicity: add step going forward
                  #If little periodicity/correlation: stop collecting new steps
                    #If enough periodicity: add step going forward
                    
                    acf_at_freq <- acf(df_temp$ma_vert[end_old:(end_new + freq + k*rounding)], lag.max = freq + k*abs(rounding))$acf[floor((freq + freq + k*rounding)/2) + 1]
                    
                    if(acf_at_freq > period_thresh){
                      step_num_fw <- step_num_fw + 1
                      
                      sequences[[seq_num]]$steps_fw[[step_num_fw]] <- c((end_new):(end_new+freq+k*rounding - 1))
                      sequences[[seq_num]]$cor_fw <- append(sequences[[seq_num]]$cor_fw, acf_at_freq)
                      
                      in_seq[(end_new):(end_new+freq+k*rounding - 1)] <- seq_num
                      in_step_fw[(end_new):(end_new+freq+k*rounding - 1)] <- step_num_fw
                      
                      peak_found <- T
                      
                      end_old <- end_new
                      end_new <- end_new + freq + k*rounding
                    } else{  #If little periodicity/correlation: stop collecting new steps 

                      forward <- F
                      
                    } 

                  break
                }
                else if (df_temp$step_valley[end_new + freq - k*rounding] == 1){
                  #If enough periodicity: add step going forward
                  #If little periodicity/correlation: stop collecting new steps
                  
                  acf_at_freq <- acf(df_temp$ma_vert[end_old:(end_new + freq - k*rounding)], lag.max = freq + k*abs(rounding))$acf[floor((freq + freq - k*rounding)/2) + 1]
                  
                  if(acf_at_freq > period_thresh){
                    step_num_fw <- step_num_fw + 1
                    
                    sequences[[seq_num]]$steps_fw[[step_num_fw]] <- c((end_new):(end_new+freq-k*rounding - 1))
                    sequences[[seq_num]]$cor_fw <- append(sequences[[seq_num]]$cor_fw, acf_at_freq)
                    
                    peak_found <- T
                    
                    in_seq[(end_new):(end_new+freq-k*rounding - 1)] <- seq_num
                    in_step_fw[(end_new):(end_new+freq-k*rounding - 1)] <- step_num_fw
                    
                    end_old <- end_new
                    end_new <- end_new + freq + k*rounding
                  } else{  #If little periodicity/correlation: stop collecting new steps 

                    forward <- F
                    
                  }   

                  break
                }
                
              }
              
              #Here handle case that after this for-loop still no peak found: also
              #stop collecting new steps
              if(peak_found == F){
                forward <- F
              }
            }
          }
          
          #Now add steps to the sequence going back
          backward <- T #Still chance of adding forward steps
          start_old <- j
          start_new <- j - bw #Current first one: we already know it is a valley
          
          while(backward){
            peak_found <- F
            
            if((start_new - freq - period_len) < 1){
              backward <- F #We are at or close to start of dataframe. Cannot add new ones
            }
            else if(sum(in_seq[(start_new - freq - period_len):(start_new - 1)]) != 0){
              backward <- F 
              
              #We do not want to overlap with another sequence of movements. Later can add that we may want
              #to merge here // throw a message, but in principle indicates a break at some point,
              #as otherwise we would have detected this one earlier
            }
            else if(df_temp$step_valley[start_new - freq] == 1){
              #If enough periodicity: add step going backward
              acf_at_freq <- acf(df_temp$ma_vert[(start_new - freq):(start_old)], lag.max = freq)$acf[freq + 1]
              
              if(acf_at_freq > period_thresh){
                step_num_bw <- step_num_bw + 1
                
                sequences[[seq_num]]$steps_bw[[step_num_bw]] <- c((start_new - freq):(start_new - 1))
                sequences[[seq_num]]$cor_bw <- append(sequences[[seq_num]]$cor_bw, acf_at_freq)
                
                in_seq[(start_new - freq):(start_new - 1)] <- seq_num
                in_step_bw[(start_new - freq):(start_new - 1)] <- step_num_bw
                
                peak_found <- T
                
                start_old <- start_new
                start_new <- start_new - freq
              } else{  #If little periodicity/correlation: stop collecting new steps 
                
                backward <- F
                
              }
            } else{ #If not at lag difference of 0, see if at any other lag
              for(k in 1:period_len){

                if (df_temp$step_valley[start_new - (freq + k*rounding)] == 1){
                  #If enough periodicity: add step going forward
                  #If little periodicity/correlation: stop collecting new steps
                  #If enough periodicity: add step going forward
                  
                  acf_at_freq <- acf(df_temp$ma_vert[(start_new - (freq + k*rounding)):(start_old)], lag.max = freq + k*abs(rounding))$acf[floor((freq + freq + k*rounding)/2) + 1]
                  
                  if(acf_at_freq > period_thresh){
                    step_num_bw <- step_num_bw + 1
                    
                    sequences[[seq_num]]$steps_bw[[step_num_bw]] <- c((start_new - (freq + k*rounding)):(start_new - 1))
                    sequences[[seq_num]]$cor_bw <- append(sequences[[seq_num]]$cor_bw, acf_at_freq)
                    
                    in_seq[(start_new - (freq + k*rounding)):(start_new - 1)] <- seq_num
                    in_step_bw[(start_new - (freq + k*rounding)):(start_new - 1)] <- step_num_bw
                    
                    peak_found <- T
                    
                    start_old <- start_new
                    start_new <- start_new - (freq + k*rounding)
                  } else{  #If little periodicity/correlation: stop collecting new steps 
                    
                    backward <- F
                    
                  }                  
                  break
                }
                  else if (df_temp$step_valley[start_new - (freq - k*rounding)] == 1){
                    #If enough periodicity: add step going forward
                    #If little periodicity/correlation: stop collecting new steps
                    
                    acf_at_freq <- acf(df_temp$ma_vert[(start_new - (freq - k*rounding)):start_old], lag.max = freq + k*abs(rounding))$acf[floor((freq + freq - k*rounding)/2) + 1]
                    
                    if(acf_at_freq > period_thresh){
                      step_num_bw <- step_num_bw + 1
                      
                      sequences[[seq_num]]$steps_bw[[step_num_bw]] <- c((start_new - (freq - k*rounding)):(start_new - 1))
                      sequences[[seq_num]]$cor_bw <- append(sequences[[seq_num]]$cor_bw, acf_at_freq)
                      
                      in_seq[(start_new - (freq - k*rounding)):(start_new - 1)] <- seq_num
                      in_step_bw[(start_new - (freq - k*rounding)):(start_new - 1)] <- step_num_bw
                      
                      peak_found <- T
                      
                      start_old <- start_new
                      start_new <- start_new - (freq - k*rounding)
                    } else{  #If little periodicity/correlation: stop collecting new steps 
                      
                      backward <- F
                      
                    }                  
                    break
                  }
                  
                }
                
                #Here handle case that after this for-loop still no peak found: also
                #stop collecting new steps
              if(peak_found == F){
                backward <- F
              }  
                
              }
          }
      }
    }
      }
    }
  } else if(df_temp$relative_frac[j] <= thresh_rel){
    break #We do not need to continue once relative freq gets too low: no steps anymore
    }
}

df_temp$in_seq <- in_seq
df_temp$in_step_fw <- in_step_fw
df_temp$in_step_bw <- in_step_bw

#Now keep testing function and see if all works as expected
test <- filter(df_temp, df_temp$in_seq == 39)
ggplot(test, aes(x= row)) + geom_line(aes(y = acc_vert))

ggplot(test, aes(x = row)) + geom_line(aes(y = in_seq/50), colour = "red") + geom_line(aes(y = acc_vert)) + 
  geom_line(aes(y = in_step_fw/3), colour = "green") +
  geom_line(aes(y = in_step_bw/3), colour = "blue")

ggplot(df_temp[5000:5200,], aes(x = row)) + geom_line(aes(y = in_seq/50), colour = "red") + geom_line(aes(y = acc_vert)) + 
  geom_line(aes(y = in_step_fw/3), colour = "green") +
  geom_line(aes(y = in_step_bw/3), colour = "blue")

sequences[[13]]$cor_bw
sequences[[13]]$cor_fw
