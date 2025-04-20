#A function that takes a dataframe with movement information
#a column number and start and end time. It returns how far
#each observation is from the mean. 
compute_rest_errors <- function(df, col_num, start_time, end_time, name){
  col <- filter(df, time > start_time & time < end_time)[,col_num]
  #Can do without name by using deparse(substitue(df)) as well
  return(data.frame(idx = 1:length(col), error = col - mean(col), df = name))
}

#...
compute_rest_dataframes <- function(names, col_num, start_times, end_times){
  dataframes <- list()
  
  j <- 1
  for(name in names) {
    assign(paste0("rest_", name), compute_rest_errors(get(name), col_num, start_times[j], end_times[j], name))
    j <- j + 1
    dataframes <- append(dataframes, list(get(paste0("rest_", name))))
    #print(max(get(paste0("rest_", name))$error))
  }
  return(dataframes)
}

#Adds the moving average of a column to a dataframe
add_ma_to_df <- function(df, col_name, col_num, n) {
  df[[col_name]] <- ma(df[col_num], n)
  return(df)
}

#Function for computing the offset automatically
compute_and_verify_offsets <- function(df, four_starts, four_ends, g_cor, min_dist_thresh, var_thresh, final_thresh) {
  vec1 <- c(filter(df, time > four_starts[1] & time < four_ends[1])[,2] %>% mean(),
            filter(df, time > four_starts[1] & time < four_ends[1])[,3] %>% mean(),
            filter(df, time > four_starts[1] & time < four_ends[1])[,4] %>% mean())
  vec2 <- c(filter(df, time > four_starts[2] & time < four_ends[2])[,2] %>% mean(),
            filter(df, time > four_starts[2] & time < four_ends[2])[,3] %>% mean(),
            filter(df, time > four_starts[2] & time < four_ends[2])[,4] %>% mean())
  vec3 <- c(filter(df, time > four_starts[3] & time < four_ends[3])[,2] %>% mean(),
            filter(df, time > four_starts[3] & time < four_ends[3])[,3] %>% mean(),
            filter(df, time > four_starts[3] & time < four_ends[3])[,4] %>% mean())
  vec4 <- c(filter(df, time > four_starts[4] & time < four_ends[4])[,2] %>% mean(),
            filter(df, time > four_starts[4] & time < four_ends[4])[,3] %>% mean(),
            filter(df, time > four_starts[4] & time < four_ends[4])[,4] %>% mean())
  
  
  distances <- as.matrix(dist(rbind(vec1, vec2, vec3, vec4)))
  diag(distances) <- Inf
  min_distance <- min(distances)
  if(min_distance < min_dist_thresh){print("Vectors are too close")}
  
  max_var <- max(filter(df, time > four_starts[1] & time < four_ends[1])[,2] %>% var(),
                 filter(df, time > four_starts[1] & time < four_ends[1])[,3] %>% var(),
                 filter(df, time > four_starts[1] & time < four_ends[1])[,4] %>% var(),
                 filter(df, time > four_starts[2] & time < four_ends[2])[,2] %>% var(),
                 filter(df, time > four_starts[2] & time < four_ends[2])[,3] %>% var(),
                 filter(df, time > four_starts[2] & time < four_ends[2])[,4] %>% var(),
                 filter(df, time > four_starts[3] & time < four_ends[3])[,2] %>% var(),
                 filter(df, time > four_starts[3] & time < four_ends[3])[,3] %>% var(),
                 filter(df, time > four_starts[3] & time < four_ends[3])[,4] %>% var(),
                 filter(df, time > four_starts[4] & time < four_ends[4])[,2] %>% var(),
                 filter(df, time > four_starts[4] & time < four_ends[4])[,3] %>% var(),
                 filter(df, time > four_starts[4] & time < four_ends[4])[,4] %>% var()
                 )
  if(max_var > var_thresh) {print("Variance is too large")}
  
  observations <- rbind(vec1, vec2, vec3)
  
  #Defined within other function because some values needed
  residuals <- function(offsets) {
    x_offset <- offsets[1]
    y_offset <- offsets[2]
    z_offset <- offsets[3]
    
    apply(observations, 1, function(row) {
      x <- row[1]
      y <- row[2]
      z <- row[3]
      (x + x_offset)^2 + (y + y_offset)^2 + (z + z_offset)^2 - g_cor^2
    })
  }
  
  initial_guess <- c(0, 0, 0)
  solution <- nleqslv(x = initial_guess, fn = residuals)
  solution$x
  
  new_means <- vec4 + solution$x
  if(abs(norm(new_means, type = "2") - g_cor) > final_thresh){print("Correction does not work")}
  
  print(paste0("Final difference in norm with expected: ", norm(new_means, type = "2") - g_cor))
  
  return(solution$x)
}

#Function for computing the offset automatically
compute_and_verify_offsets_alt <- function(df, four_starts, four_ends, g_cor, min_dist_thresh, var_thresh, final_thresh) {
  vec1 <- c(filter(df, time > four_starts[1] & time < four_ends[1])[,2] %>% mean(),
            filter(df, time > four_starts[1] & time < four_ends[1])[,3] %>% mean(),
            filter(df, time > four_starts[1] & time < four_ends[1])[,4] %>% mean())
  vec2 <- c(filter(df, time > four_starts[2] & time < four_ends[2])[,2] %>% mean(),
            filter(df, time > four_starts[2] & time < four_ends[2])[,3] %>% mean(),
            filter(df, time > four_starts[2] & time < four_ends[2])[,4] %>% mean())
  vec3 <- c(filter(df, time > four_starts[3] & time < four_ends[3])[,2] %>% mean(),
            filter(df, time > four_starts[3] & time < four_ends[3])[,3] %>% mean(),
            filter(df, time > four_starts[3] & time < four_ends[3])[,4] %>% mean())
  vec4 <- c(filter(df, time > four_starts[4] & time < four_ends[4])[,2] %>% mean(),
            filter(df, time > four_starts[4] & time < four_ends[4])[,3] %>% mean(),
            filter(df, time > four_starts[4] & time < four_ends[4])[,4] %>% mean())
  
  
  distances <- as.matrix(dist(rbind(vec1, vec2, vec3, vec4)))
  diag(distances) <- Inf
  min_distance <- min(distances)
  if(min_distance < min_dist_thresh){print("Vectors are too close")}
  
  max_var <- max(filter(df, time > four_starts[1] & time < four_ends[1])[,2] %>% var(),
                 filter(df, time > four_starts[1] & time < four_ends[1])[,3] %>% var(),
                 filter(df, time > four_starts[1] & time < four_ends[1])[,4] %>% var(),
                 filter(df, time > four_starts[2] & time < four_ends[2])[,2] %>% var(),
                 filter(df, time > four_starts[2] & time < four_ends[2])[,3] %>% var(),
                 filter(df, time > four_starts[2] & time < four_ends[2])[,4] %>% var(),
                 filter(df, time > four_starts[3] & time < four_ends[3])[,2] %>% var(),
                 filter(df, time > four_starts[3] & time < four_ends[3])[,3] %>% var(),
                 filter(df, time > four_starts[3] & time < four_ends[3])[,4] %>% var(),
                 filter(df, time > four_starts[4] & time < four_ends[4])[,2] %>% var(),
                 filter(df, time > four_starts[4] & time < four_ends[4])[,3] %>% var(),
                 filter(df, time > four_starts[4] & time < four_ends[4])[,4] %>% var()
  )
  if(max_var > var_thresh) {print("Variance is too large")}
  
  observations <- rbind(vec1, vec2, vec3, vec4)
  
  #Defined within other function because some values needed
  residuals <- function(offsets) {
    x_offset <- offsets[1]
    y_offset <- offsets[2]
    z_offset <- offsets[3]
    new_len <- offsets[4]
    
    apply(observations, 1, function(row) {
      x <- row[1]
      y <- row[2]
      z <- row[3]
      sqrt((x + x_offset)^2 + (y + y_offset)^2 + (z + z_offset)^2) - new_len
    })
  }
  
  initial_guess <- c(0, 0, 0, 1) 
  #Last chosen as 1 for speed, but solution generally unique, so this is no "hint"
  solution <- nleqslv(x = initial_guess, fn = residuals)
  solution$x
  
  new_len <- solution$x[4]
  
  if(abs(g_cor - new_len) > final_thresh){print("Correction does not work")}
  
  print(paste0("Old length vec1: ", norm(vec1, type = "2")))
  print(paste0("Old length vec2: ", norm(vec2, type = "2")))
  print(paste0("Old length vec3: ", norm(vec3, type = "2")))
  print(paste0("Old length vec4: ", norm(vec4, type = "2")))
  print(paste0("Applied offsets: ", solution$x))
  print(paste0("New length vec1: ", norm(vec1 + solution$x[1:3], type = "2")))
  print(paste0("New length vec2: ", norm(vec2 + solution$x[1:3], type = "2")))
  print(paste0("New length vec3: ", norm(vec3 + solution$x[1:3], type = "2")))
  print(paste0("New length vec4: ", norm(vec4 + solution$x[1:3], type = "2")))
  
  print(paste0("Final difference in norm with expected: ", new_len - g_cor))
  print("-----------------")
  
  return(solution$x[1:3])
}
