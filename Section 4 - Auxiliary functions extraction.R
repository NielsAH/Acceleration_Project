#Computes moving average of a list/vector
ma <- function(vec, n){stats::filter(vec, rep(1/n, n), sides = 2)}

#We also need a moving average function that preserves the vector length
#Instead of outputting NA at the edges, it just takes the average over
#as many values as possible
ma_trunc <- function(vec,n){
  
  len <- length(vec)
  result <- numeric(len)
  
  for (i in 1:len) {
    left <- max(1, i - floor(n / 2))
    right <- min(len, i + floor(n / 2))
    
    result[i] <- mean(vec[left:right])
  }
  
  return(result)

}

#Lastly MA where values closer to the edge gain less weight
ma_gauss <- function(vec, n, sigma = 1) {
  len <- length(vec)
  result <- numeric(len)
  
  for (i in 1:len) {
    left <- max(1, i - floor(n / 2))
    right <- min(len, i + floor(n / 2))
    window <- vec[left:right]
    
    # Compute distances from the center
    distances <- seq(from = left, to = right) - i
    
    # Compute Gaussian weights
    weights <- exp(- (distances^2) / (2 * sigma^2))
    weights <- weights / sum(weights) 
    
    # Compute weighted mean
    result[i] <- sum(window * weights)
  }
  
  return(result)
}
#When sigma very low, no moving average. When sigma very high, weights just 1/n for each value


#Computer the energy / normalised FFT components
energy <- function(vec) {
  
  #'*Fix the even length problems below! Only at boundary problem as window otherwise odd length*
  #if(!length(vec) %% 2){ print("Beware: input vector has even length, normalisation incorrect.")}
  #Temporary, fix later to also work for even length input, if necessary !!!
  
  
  energy_spec <- vec %>% fft() %>% Mod()
  
  energy_spec <- energy_spec[2:((length(vec) +1)/2)]^2/((length(vec)-1)*(length(vec)/2))
  #Why this strange division by length minus 1 and then by length to get right connection to variance?
  #Check this, but I know it works now -> Have explained in Section 2
  #See stack exchange: seems 1 over square, but then summed over just T-1 components because first is of course mean
  return(energy_spec)
}

#MA of these energy values in the spirit of Danniell Kernel
energy_kernel_2 <- function(vec, n, kernel, ...) {
  
  #'*Fix later if there is even length! (Still does not fully work then, but approximate. Fine for figures, not necessary computations)*
  #if(!length(vec) %% 2){ print("Beware: input vector has even length, normalisation incorrect.")}
  
  energy_spec <- vec %>% fft() %>% Mod()
  
  energy_spec <- energy_spec[2:((length(vec) +1)/2)]^2/((length(vec)-1)*(length(vec)/2))
  
  energy_spec <- kernel(energy_spec, n = n, ...)
  
  return(energy_spec)
}

energy_kernel <- function(vec, n, kernel, ...) {
  
  energy_spec <- vec %>% fft() %>% Mod()
  
  energy_spec <- energy_spec[2:((length(vec) +1)/2)]^2
  
  energy_spec <- kernel(energy_spec, n = n, ...)
  
  energy_spec <- energy_spec/sum(energy_spec)
  
  return(energy_spec)
}

#Capture a number of hills. Often we need two, as we consider two main frequencies
capture_hills <- function(spec, num){
  var <- sum(spec)
  hills <- list()
  absolute_frac <- c()
  relative_frac <- c()
  frequencies <- list()
  
  for(j in 1:num){
    max <- which.max(spec)
    idc <- c(max)

    #First check on right side
    stop <- F
    i <- 0
    while(!stop){
      
      #Check if we are not getting out of the spectrum, and if not if still decreasing
      if((max + i + 1) > length(spec)){
        stop <- T
      }
      else if(spec[max + i + 1] < spec[max + i]){
        idc <- append(idc, max + i + 1)
        i <- i + 1
      } else {
        stop <- T
      }
    }
    
    #Now check on left side
    stop <- F
    i <- 0
    while(!stop){
      
      #Check if we are not getting out of the spectrum, and if not if still decreasing
      if((max - i - 1) < 1){
        stop <- T
      }
      else if(spec[max - i - 1] < spec[max - i]){
        idc <- append(idc, max - i - 1)
        i <- i+1
      } else {
        stop <- T
      }
    }
    
    #Save hill indices and size of hills
    hills[[j]] <- idc
    absolute_frac <- append(absolute_frac, sum(spec[idc]))
    relative_frac <- append(relative_frac, sum(spec[idc])/(var))
    
    #Save weighted average of period (not of frequency, is a choice)
    freq <- sum(spec[idc]/(sum(spec[idc]))*((length(spec)*2+1)/idc))
    frequencies <- append(frequencies, freq)
    
    #Make indices that are in this hill zero in the spectrum
    spec[idc] <- 0
  }
  
  return(list(hills, frequencies, relative_frac, absolute_frac))
}

rotation_initial <- function(rest_vec){
  gravity <- sign(rest_vec[2])*norm(rest_vec, type = "2")
  goal_vec <- c(0, gravity, 0) #Could also write (0,-1,0) directly, outer product is normalised later
  axis <- pracma::cross(rest_vec, goal_vec)
  angle <- acos(rest_vec[2]/gravity)
  
  unit_axis <- axis/norm(axis, type = "2")
  u1 <- unit_axis[1]
  u2 <- unit_axis[2]
  u3 <- unit_axis[3]
  
  rot_mat <- matrix(0, nrow = 3, ncol = 3)
  rot_mat[1,1] = u1^2* (1 - cos(angle))+ cos(angle)
  rot_mat[2,1] = u1*u2*(1 - cos(angle)) + u3*sin(angle)
  rot_mat[3,1] = u1*u3*(1 - cos(angle)) - u2*sin(angle)
  rot_mat[1,2] = u1*u2*(1 - cos(angle)) - u3 * sin(angle)
  rot_mat[2,2] = u2^2* (1 - cos(angle)) + cos(angle)
  rot_mat[3,2] = u2*u3*(1 - cos(angle)) + u1*sin(angle)
  rot_mat[1,3] = u1*u3*(1 - cos(angle)) + u2*sin(angle)
  rot_mat[2,3] = u2*u3*(1 - cos(angle)) - u1*sin(angle)
  rot_mat[3,3] = u3^2* (1 - cos(angle)) + cos(angle)
  
  #print(rot_mat %*% rest_vec)
  
  return(list(rot_mat, gravity))
}

rotation_horizontal <- function(x_z_angle){
  rot_mat <- matrix(0, nrow = 3, ncol = 3)
  
  rot_mat[1,1] = cos(x_z_angle)
  rot_mat[2,1] = 0
  rot_mat[3,1] = -sin(x_z_angle)
  rot_mat[1,2] = 0
  rot_mat[2,2] = 1
  rot_mat[3,2] = 0
  rot_mat[1,3] = sin(x_z_angle)
  rot_mat[2,3] = 0
  rot_mat[3,3] = cos(x_z_angle)
  
  return(rot_mat)
}
