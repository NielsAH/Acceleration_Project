periodicity_calculator <- function(df, col_num, start_idx, stop_idx, wdw_len, wdw_ma) {
  
  periodicity_values <- c()
  
  for(j in start_idx:stop_idx)
  {
    window <- ma(df[(j - wdw_len - (wdw_ma-1)/2):(j + wdw_len + (wdw_ma-1)/2), col_num],wdw_ma)
    window <- window[((wdw_ma+1)/2):(2*wdw_len + (wdw_ma+1)/2)]
    spectrum <- window %>% fft() %>% Mod()
    spectrum <- spectrum[2:(2*wdw_len+1)] #Excluding DC component here already works a lot easier with indices
    spectrum_idx <- spectrum %>% order(decreasing = T)
    periodicity <- (spectrum[spectrum_idx][1:2]^2 %>% sum()) / (spectrum[spectrum_idx][3:(2*wdw_len)]^2 %>% sum())
    
    periodicity_values <- append(periodicity_values, periodicity)
  
  }
  
  return(periodicity_values)
  
}

abs_periodicity_calculator <- function(df, col_num, start_idx, stop_idx, wdw_len, wdw_ma) {
  
  periodicity_values <- c()
  
  for(j in start_idx:stop_idx)
  {
    window <- ma(df[(j - wdw_len - (wdw_ma-1)/2):(j + wdw_len + (wdw_ma-1)/2), col_num],wdw_ma)
    window <- window[((wdw_ma+1)/2):(2*wdw_len + (wdw_ma+1)/2)]
    spectrum <- window %>% fft() %>% Mod()
    spectrum <- spectrum[2:(2*wdw_len+1)] #Excluding DC component here already works a lot easier with indices
    spectrum_idx <- spectrum %>% order(decreasing = T)
    periodicity <- max(spectrum)/(length(spectrum))
    
    periodicity_values <- append(periodicity_values, periodicity)
    
  }
  
  return(periodicity_values)
  
}

abs_periodicity_calculator_vec <- function(vector, wdw_len) {
  periodicity_values <- c()
  
  for(j in (wdw_len + 1):(length(vector) - wdw_len)){
    spec <- fft(vector[(j - wdw_len):(j + wdw_len)]) %>% Mod()
    spec_relevant <- spec[2:(wdw_len+1)]
    freq <- (2*wdw_len)/(which.max(spec_relevant))
    main_freq <- append(main_freq, freq)
  }
  
  return(main_freq)

}

frequency_calculator_vec <- function(vector, wdw_len) {

    main_freq <- c()

  for(j in (wdw_len + 1):(length(vector) - wdw_len)){
    spec <- fft(vector[(j - wdw_len):(j + wdw_len)]) %>% Mod()
    spec_relevant <- spec[2:(wdw_len+1)]
    freq <- (2*wdw_len)/(which.max(spec_relevant))
    main_freq <- append(main_freq, freq)
  }
    
  return(main_freq)
  
}

#Meant for and works for odd input length
alt_fft <- function(vector) {
  
  if(!length(vector) %% 2){ print("Beware: input vector has even length, normalisation incorrect.")}
  #Temporary, fix later to also work for even length input, if necessary
  
  alt_spec <- vector %>% fft() %>% Mod()
  
  alt_spec <- alt_spec[2:((length(vector) +1)/2)]^2/((length(vector)-1)*(length(vector)/2))
  #Why this strange division by length minus 1 and then by length to get right connection to variance?
  #Check this, but I know it works now
  #See stack exchange: seems 1 over square, but then summed over just T-1 components because first is of course mean
  return(alt_spec)
}

determine_hills <- function(ma_wdw, alt_spec, hill_thresh, hill_significance) {
  #Input spec needs to be an alt_spec attained through alt_fft
  
  mean_val <- 1/length(alt_spec) # = mean(spec)
  ma_range <- (ma_wdw-1)/2 #ma_wdw assumed to be odd, put in test still
  
  hills <- list()
  hills_to_check <- TRUE
  hammered_spec <- alt_spec
  
  while(hills_to_check == TRUE){
    spec_ma <- ma(hammered_spec, n = ma_wdw)
    spec_ma_left <- ma_left(hammered_spec, n = ma_range)
    spec_ma_right <- ma_right(hammered_spec, n = ma_range + 1)
    #Document why the +1 at one, and not at the other. Just an arbitrary choice to ensure we do not count any value for both left_ma and right_ma when comparing\
    peak_ma_idx <- which.max(spec_ma)
    peak_ma <- spec_ma[peak_ma_idx]
    
    hill_ma <- c(peak_ma_idx)
    
    #First determine how many values to include on left side
    j <- 0
    over_thresh = TRUE
    
    while(over_thresh){
      j <- j + 1
      
      if(is.na(spec_ma[peak_ma_idx - j])){
        over_thresh = FALSE
      } else if(spec_ma[peak_ma_idx - j] > hill_thresh){
        hill_ma <- append(hill_ma, peak_ma_idx - j)
      } else{
        over_thresh = FALSE
      }
    }
    
    #Then determine how many to include on right side
    j <- 0
    over_thresh = TRUE
    
    while(over_thresh){
      j <- j + 1
      
      if(is.na(spec_ma[peak_ma_idx + j])){
        over_thresh = FALSE
      } else if(spec_ma[peak_ma_idx + j] > hill_thresh){
        hill_ma <- append(hill_ma, peak_ma_idx + j)
      }
      else{
        over_thresh = FALSE
      }
    }
    
    #Trim on left side
    start_idx <- min(hill_ma)
    
    if(start_idx == (ma_range + 1)){ #Built in to ensure no left_ma is taken when not possible
      new_start_idx <- start_idx
    } else{
      possible_start_idx <- (min(hill_ma) - ma_range):(min(hill_ma) + ma_range)
      start_idx_adjust <- which.max(spec_ma_right[possible_start_idx] - spec_ma_left[possible_start_idx - 1])
      new_start_idx <- start_idx - (ma_range + 1) + start_idx_adjust #Worked in old case without accounting for edges
    }
    
    #Trim on right side
    stop_idx <- max(hill_ma)
    
    if(stop_idx == (length(alt_spec) - ma_range)){
      new_stop_idx <- stop_idx
    } else {
      possible_stop_idx <- (max(hill_ma) - ma_range):(max(hill_ma) + ma_range)
      stop_idx_adjust <- which.min(spec_ma_right[possible_stop_idx + 1] - spec_ma_left[possible_stop_idx])
      new_stop_idx <- stop_idx - (ma_range + 1) + stop_idx_adjust 
    }
    
    #Combine trims
    hill <- new_start_idx:new_stop_idx
    
    #Determine if hill is large enough. If not, we stop checking new hills
    #Note we can in theory skip hills that are significant by doing it like this, but
    #as they have lower moving-average peaks that insignificant hills, this
    #will never be a big problem
    if(sum(hammered_spec[hill]) >= hill_significance){
      hills <- append(hills, list(hill))
      hammered_spec[new_start_idx:new_stop_idx] <- 0
    } else if(sum(hammered_spec[hill]) < hill_significance){
      hills_to_check = FALSE
    }
  }
  
  return(hills)
  
}

get_hill_frequencies <- function(spec, hills, N){
  movement_frequencies <- c()
  
  for(hill in hills){
    hill_values <- spec[hill]
    hill_centre <- (min(hill) - 1 + (sum(hill_values*(1:length(hill_values)))/sum(hill_values))) #Can round, but do not even have to I think
    hill_freq <- (N)/hill_centre #We divide by hill_centre and not hill_centre minus 1, as we already removed DC component in computing the alt_spec
    movement_frequencies <- append(movement_frequencies, hill_freq)
  }
  
  return(movement_frequencies)
}
