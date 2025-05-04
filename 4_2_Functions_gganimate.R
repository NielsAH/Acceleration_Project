generate_data <- function(folder_name,
                          width, 
                          start_time, 
                          raw_df,
                          wdw_size,
                          ma_data,
                          ma_energy,
                          kernel_func,
                          std_dev,
                          upright_thresh,
                          abs_acc_thresh,
                          thresh_abs,
                          thresh_rel,
                          min_step_period,
                          max_step_period,
                          period_len,
                          period_len_double,
                          period_thresh,
                          double_step_thresh,
                          thresh_direction){
  
  vid_frames <- length(list.files(paste0("frames/", folder_name, "/video"), pattern = "\\.jpg")) 
  
  end_time <- start_time - (1/30) + vid_frames/30 + (width - 1)*(1/30) 
  df <- filter(raw_df, time >= start_time & time <= end_time) 
  
  unordered_seq <- fast_extraction_algorithm(df, 
                                             wdw_size, 
                                             ma_data, 
                                             ma_energy, 
                                             kernel_func, 
                                             upright_thresh, 
                                             abs_acc_thresh, 
                                             thresh_abs, 
                                             thresh_rel, 
                                             min_step_period, 
                                             max_step_period, 
                                             period_len,
                                             period_len_double, 
                                             period_thresh, 
                                             double_step_thresh, 
                                             thresh_direction, 
                                             sigma = std_dev)
  
  ordered_seq <- unordered_seq[order(sapply(unordered_seq, function(x) x$data$time[1]))]
  
  return(ordered_seq)
}

#No filter and also no use of overarch
df_all_from_mvm_alt_nofilter <- function(data, n, start_function){
  all_fw_steps <- list()
  all_vert_steps <- list()
  all_side_steps <- list()
  
  step_data <- list(mvm = c())
  
  mvm_num <- 0
  
  for(mvm in data){
    mvm_fw_steps <- list()
    mvm_vert_steps <- list()
    mvm_side_steps <- list()
    
      end_bw <- max(mvm$data$in_step_bw)
      end_fw <- max(mvm$data$in_step_fw)
      
      for(j in 1:end_bw){
        step <- filter(mvm$data, in_step_bw == j)
        
        new_fw <- rescale(step$acc_fw, n)
        new_vert <- rescale(step$acc_vert, n)
        new_side <- rescale(step$acc_side, n)
        
        mvm_fw_steps <- append(mvm_fw_steps, list(new_fw))
        mvm_vert_steps <- append(mvm_vert_steps, list(new_vert))
        mvm_side_steps <- append(mvm_side_steps, list(new_side))
      }
      
      for(j in 1:end_fw){
        step <- filter(mvm$data, in_step_fw == j)
        
        new_fw <- rescale(step$acc_fw, n)
        new_vert <- rescale(step$acc_vert, n)
        new_side <- rescale(step$acc_side, n)
        
        mvm_fw_steps <- append(mvm_fw_steps, list(new_fw))
        mvm_vert_steps <- append(mvm_vert_steps, list(new_vert))
        mvm_side_steps <- append(mvm_side_steps, list(new_side))
      }
      
      mvm_fw_avg <- numeric(n)
      mvm_vert_avg <- numeric(n)
      mvm_side_avg <- numeric(n)
    
    if(length(mvm_fw_steps) > 0){
      for(j in 1:length(mvm_fw_steps)){
        mvm_fw_avg <- mvm_fw_avg + mvm_fw_steps[[j]]
        mvm_vert_avg <- mvm_vert_avg + mvm_vert_steps[[j]]
        mvm_side_avg <- mvm_side_avg + mvm_side_steps[[j]]
      }
      mvm_fw_avg <- mvm_fw_avg/length(mvm_fw_steps)
      mvm_vert_avg <- mvm_vert_avg/length(mvm_fw_steps)
      mvm_side_avg <- mvm_side_avg/length(mvm_fw_steps)
      
      if(mvm_num > 0){
        start_index <- start_function(list(mvm_fw_avg, 
                                           mvm_vert_avg, 
                                           mvm_side_avg,
                                           avg_fw_steps_sofar,
                                           avg_vert_steps_sofar,
                                           avg_side_steps_sofar)) 
      }
      else{
        start_index <- which.max(mvm_vert_avg)
      }
      
      #Now we cycle every rescaled step in mvm_steps individually and add them to the df
      for(step in mvm_fw_steps){
        if(start_index != 1){
          step <-  c(step[start_index:n], step[1:(start_index-1)])
        }
        all_fw_steps <- append(all_fw_steps, list(step))
        step_data$mvm <- append(step_data$mvm, mvm_num)
      }
      for(step in mvm_vert_steps){
        if(start_index != 1){
          step <-  c(step[start_index:n], step[1:(start_index-1)])
        }
        all_vert_steps <- append(all_vert_steps, list(step))
      }
      for(step in mvm_side_steps){
        if(start_index != 1){
          step <-  c(step[start_index:n], step[1:(start_index-1)])
        }
        all_side_steps <- append(all_side_steps, list(step))
      }
    }
    
    avg_fw_steps_sofar <- numeric(n)
    avg_vert_steps_sofar <- numeric(n)
    avg_side_steps_sofar <- numeric(n)
    
    #Compute average so far from e.g. all_fw_steps we have right now. Used in next cycle
    if(length(all_fw_steps) > 0){
      for(i in 1:length(all_fw_steps)){
        for(j in 1:n){
          avg_fw_steps_sofar[j] <- avg_fw_steps_sofar[j] + all_fw_steps[[i]][j]
          avg_vert_steps_sofar[j] <- avg_vert_steps_sofar[j] + all_vert_steps[[i]][j]
          avg_side_steps_sofar[j] <- avg_side_steps_sofar[j] + all_side_steps[[i]][j]
        }
      }
      avg_fw_steps_sofar <- avg_fw_steps_sofar/length(all_fw_steps)
      avg_vert_steps_sofar <- avg_vert_steps_sofar/length(all_fw_steps)
      avg_side_steps_sofar <- avg_side_steps_sofar/length(all_fw_steps)
      
      mvm_num <- mvm_num + 1
    }
  }
  fw_df <- rbindlist(lapply(all_fw_steps, as.list))
  vert_df <- rbindlist(lapply(all_vert_steps, as.list))
  side_df <- rbindlist(lapply(all_side_steps, as.list))
  #side_df <- as.data.frame(do.call(rbind, all_steps))
  return(list(fw_df, vert_df, side_df, step_data))
}

#Key function!
df_all_from_mvm <- function(data, n, start_function, filter_function, filter_par){
  all_fw_steps <- list()
  all_vert_steps <- list()
  all_side_steps <- list()
  
  for(mvm in data){
    mvm_fw_steps <- list()
    mvm_vert_steps <- list()
    mvm_side_steps <- list()
    
    if(mvm$movement_type == "Walk" & filter_function(mvm, filter_par)){
      
      end_bw <- max(mvm$data$in_step_bw)
      end_fw <- max(mvm$data$in_step_fw)
      
      for(j in 1:end_bw){
        step <- filter(mvm$data, in_step_bw == j)
        
        new_fw <- rescale(step$acc_fw, n)
        new_vert <- rescale(step$acc_vert, n)
        new_side <- rescale(step$acc_side, n)
        
        mvm_fw_steps <- append(mvm_fw_steps, list(new_fw))
        mvm_vert_steps <- append(mvm_vert_steps, list(new_vert))
        mvm_side_steps <- append(mvm_side_steps, list(new_side))
      }
      
      for(j in 1:end_fw){
        step <- filter(mvm$data, in_step_fw == j)
        
        new_fw <- rescale(step$acc_fw, n)
        new_vert <- rescale(step$acc_vert, n)
        new_side <- rescale(step$acc_side, n)
        
        mvm_fw_steps <- append(mvm_fw_steps, list(new_fw))
        mvm_vert_steps <- append(mvm_vert_steps, list(new_vert))
        mvm_side_steps <- append(mvm_side_steps, list(new_side))
      }
      
      mvm_fw_avg <- numeric(n)
      mvm_vert_avg <- numeric(n)
      mvm_side_avg <- numeric(n)
    }
    
    if(length(mvm_fw_steps) > 0){
      for(j in 1:length(mvm_fw_steps)){
        mvm_fw_avg <- mvm_fw_avg + mvm_fw_steps[[j]]
        mvm_vert_avg <- mvm_vert_avg + mvm_vert_steps[[j]]
        mvm_side_avg <- mvm_side_avg + mvm_side_steps[[j]]
      }
      start_index <- start_function(list(mvm_fw_avg, mvm_vert_avg, mvm_side_avg))
      #Now we cycle every rescaled step in mvm_steps individually and add them to the df
      for(step in mvm_fw_steps){
        if(start_index != 1){
          step <-  c(step[start_index:n], step[1:(start_index-1)])
        }
        all_fw_steps <- append(all_fw_steps, list(step))
      }
      for(step in mvm_vert_steps){
        if(start_index != 1){
          step <-  c(step[start_index:n], step[1:(start_index-1)])
        }
        all_vert_steps <- append(all_vert_steps, list(step))
      }
      for(step in mvm_side_steps){
        if(start_index != 1){
          step <-  c(step[start_index:n], step[1:(start_index-1)])
        }
        all_side_steps <- append(all_side_steps, list(step))
      }
    }
  }
  fw_df <- rbindlist(lapply(all_fw_steps, as.list))
  vert_df <- rbindlist(lapply(all_vert_steps, as.list))
  side_df <- rbindlist(lapply(all_side_steps, as.list))
  #side_df <- as.data.frame(do.call(rbind, all_steps))
  return(list(fw_df, vert_df, side_df))
}

generate_images_avg <- function(steps_info, alpha_val, folder_name){
  n <- ncol(steps_info[[1]])
  colours <- c("#56B4E9", "#D62728", "#FFCC33")
  names <- c("Forward", "Vertical", "Sideways")
  file_names <- c("avg_stream1", "avg_stream2", "avg_stream3")
  
  for(j in 1:3){
    temp_df <- steps_info[[j]]
    colnames(temp_df) <- as.character(1:n)
    temp_df$id <- 1:nrow(temp_df)
    temp_melted = melt(temp_df, id.vars = 'id')
    temp_means <- aggregate(value ~ variable, data = temp_melted, FUN = mean)
    
    temp_melted$variable <- as.numeric(as.character(temp_melted$variable))
    temp_means$variable <- as.numeric(as.character(temp_means$variable))
    
    plot <- ggplot(temp_melted, aes(x = variable, y = value)) + 
      geom_line(aes(group = id), color = colours[j], linewidth = 0.05, alpha = alpha_val) +
      geom_line(data = temp_means, aes(x = variable, y = value, group = 1), 
                color = "black", linewidth = 0.5) +
      labs(
        x = "Index",
        y = TeX(r'( Acceleration in $m/s^2$ )'),
        title = paste0("Average step ", names[j])) +
      theme_minimal() +
      theme(
        text = element_text(size = 12),      
        axis.title = element_text(size = 12),   
        axis.text = element_text(size = 12),      
        legend.text = element_text(size = 12),     
        legend.title = element_text(size = 12),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()
      )
    
    ggsave(paste0("frames/", folder_name, "/Averages/", file_names[j] ,".png"), plot, width = 12, height = 4, dpi = 600)
  }
}

generate_images <- function(folder_name,
                            width, 
                            start_time, 
                            fw_bottom,
                            fw_top,
                            vert_bottom,
                            vert_top,
                            side_bottom,
                            side_top,
                            fig_width,
                            fig_height,
                            fix_y,
                            ordered_seq){
  
  vid_frames <- length(list.files(paste0("frames/", folder_name, "/video"), pattern = "\\.jpg")) 
  
  end_time <- start_time - (1/30) + vid_frames/30 + (width - 1)*(1/30) 
  
  temp <- data.frame()
  mvm_num <- 0
  for(j in ordered_seq){
    mvm_num <- mvm_num + 1
    mvm_df <- j$data %>% mutate(mvm = mvm_num)
    temp <- rbind(temp, mvm_df)
  }
  temp <- temp %>% select(time, acc_fw, acc_vert, acc_side, in_step_bw, in_step_fw, mvm)
  temp$time <- as.numeric(temp$time)
  last_mvm <- max(temp$mvm)
  
  if(last_mvm > 1){
    for(j in 1:(last_mvm - 1)){
      insert_after <- max(which(temp$mvm == j))
      time_before <- temp$time[insert_after]
      time_after <- temp$time[insert_after + 1]
      row_number <- round((time_after - time_before)*30) - 1
      for(k in 1:row_number){
        temp <- temp %>% add_row(time = time_before + (1/30)*k, acc_fw = 0, acc_vert = 0, acc_side = 0, in_step_bw = 0, in_step_fw = 0, mvm = 0, .after = insert_after - 1 + k)
      }
    } 
  }
  
  insert_before <- 1
  time_before <- as.numeric(start_time) - 1/30 
  time_after <- temp$time[1]
  row_number <- round((time_after - time_before)*30) - 1
  row_number <- round((time_after - time_before)*30) - 1
  for(k in 1:row_number){
    temp <- temp %>% add_row(time = time_after - (1/30)*k, acc_fw = 0, acc_vert = 0, acc_side = 0, in_step_bw = 0, in_step_fw = 0, mvm = 0, .before = insert_before)
  }
  
  insert_after <- nrow(temp)
  time_before <- temp$time[nrow(temp)] 
  time_after <- as.numeric(end_time) + 1/30
  row_number <- round((time_after - time_before)*30) - 1
  for(k in 1:row_number){
    temp <- temp %>% add_row(time = time_before + (1/30)*k, acc_fw = 0, acc_vert = 0, acc_side = 0, in_step_bw = 0, in_step_fw = 0, mvm = 0, .after = insert_after - 1 + k)
  }
  
  half_width <- (width - 1)/2
  frame_tot <- nrow(temp) + 1 - width
  
  frame_list <- c()
  for(j in 1:frame_tot){
    frame_list <- append(frame_list, rep(j, width))
  }
  
  idx_list <- c()
  for(j in 1:frame_tot){
    idx_list <- append(idx_list, temp$time[j:(j + width - 1)])
  }
  
  val_list_vert <- c()
  for(j in 1:frame_tot){
    val_list_vert <- append(val_list_vert, temp$acc_vert[ j:(j + width - 1)])
  }
  
  val_list_fw <- c()
  for(j in 1:frame_tot){
    val_list_fw <- append(val_list_fw, temp$acc_fw[ j:(j + width - 1)])
  }
  
  val_list_side <- c()
  for(j in 1:frame_tot){
    val_list_side <- append(val_list_side, temp$acc_side[ j:(j + width - 1)])
  }
  
  colour_function <- function(df, idx){
    if((df$in_step_bw[idx] == 0) & (df$in_step_fw[idx] == 0)){
      return(2)
    }
    else if((df$in_step_bw[idx] %% 2 == 1) | (df$in_step_fw[idx] %% 2 == 0 & df$in_step_fw[idx] != 0)){ #Changed second from 1 to 0
      return(3)
    }
    else{
      return(4)
    }
  }
  
  alt_colour_list <- c()
  for(j in 1:frame_tot){
    if(j %% 500 == 0){
      print(paste0("Progress: ", j/frame_tot))
    }
    for(k in 1:half_width){
      alt_colour_list <- append(alt_colour_list, colour_function(temp, j - 1 + k))
    }
    alt_colour_list <- append(alt_colour_list, 1)
    for(k in 1:half_width){
      alt_colour_list <- append(alt_colour_list, colour_function(temp, j + half_width + k))
    }
  }
  
  new <- data.frame(frame = frame_list, idx = idx_list, 
                    value_fw = val_list_fw, 
                    value_vert = val_list_vert, 
                    value_side = val_list_side,
                    colour = alt_colour_list)
  
  no_of_thousands <- max(new$frame) %/% 1000
  
  for(l in 0:no_of_thousands){
    
    frame_selection <- filter(new, frame >= 1000*l & frame < 1000*(l + 1))
    
    p_fw <- ggplot(frame_selection, aes(x = idx, y = value_fw)) +
      geom_line() +
      geom_point(aes(color = factor(colour), size = factor(colour))) +
      scale_colour_manual(values = c("1" = "black", "2" = "#984EA3", "3" = "#009E73", "4" = "#E69F00"),
                          labels = c("Current observation", "No activity", "Odd step pairs", "Even step pairs"),
                          name = "") +
      scale_size_manual(values = c("1" = 15, "2" = 5, "3" = 5, "4" = 5),
                        labels = c("Current observation", "No activity", "Odd step pairs", "Even step pairs"),
                        name = "") +
      transition_manual(frame) +
      ylim(fw_bottom, fw_top) +
      view_follow(fixed_y = fix_y) +
      labs(
        x = "Numerical time (seconds since Unix epoch)",
        y = TeX("Vertical acceleration in g (9.80665 m/s²)")
      ) +
      theme_minimal() +
      theme(
        text = element_text(size = 14),            
        axis.title = element_text(size = 14),      
        axis.text = element_text(size = 14),       
        legend.text = element_text(size = 14),     
        legend.title = element_text(size = 14)     
      )
    
    p_vert <- ggplot(frame_selection, aes(x = idx, y = value_vert)) +
      geom_line() +
      geom_point(aes(color = factor(colour), size = factor(colour))) +
      scale_colour_manual(values = c("1" = "black", "2" = "#984EA3", "3" = "#009E73", "4" = "#E69F00"),
                          labels = c("Current observation", "No activity", "Odd step pairs", "Even step pairs"),
                          name = "") +
      scale_size_manual(values = c("1" = 15, "2" = 5, "3" = 5, "4" = 5),
                        labels = c("Current observation", "No activity", "Odd step pairs", "Even step pairs"),
                        name = "") +
      transition_manual(frame) +
      ylim(vert_bottom, vert_top) +
      view_follow(fixed_y = fix_y) +
      labs(
        x = "Numerical time (seconds since Unix epoch)",
        y = TeX("Vertical acceleration in g (9.80665 m/s²)")
      ) +
      theme_minimal() +
      theme(
        text = element_text(size = 14),            
        axis.title = element_text(size = 14),      
        axis.text = element_text(size = 14),       
        legend.text = element_text(size = 14),     
        legend.title = element_text(size = 14)     
      )
    
    p_side <- ggplot(frame_selection, aes(x = idx, y = value_side)) +
      geom_line() +
      geom_point(aes(color = factor(colour), size = factor(colour))) +
      scale_colour_manual(values = c("1" = "black", "2" = "#984EA3", "3" = "#009E73", "4" = "#E69F00"),
                          labels = c("Current observation", "No activity", "Odd step pairs", "Even step pairs"),
                          name = "") +
      scale_size_manual(values = c("1" = 15, "2" = 5, "3" = 5, "4" = 5),
                        labels = c("Current observation", "No activity", "Odd step pairs", "Even step pairs"),
                        name = "") +
      transition_manual(frame) +
      ylim(side_bottom, side_top) +
      view_follow(fixed_y = fix_y) +
      labs(
        x = "Numerical time (seconds since Unix epoch)",
        y = TeX("Vertical acceleration in g (9.80665 m/s²)")
      ) +
      theme_minimal() +
      theme(
        text = element_text(size = 14),            
        axis.title = element_text(size = 14),      
        axis.text = element_text(size = 14),       
        legend.text = element_text(size = 14),     
        legend.title = element_text(size = 14)     
      )
    
    frame_num <- nrow(frame_selection)/width
    
    print(paste0("Progress: ", l/no_of_thousands))
    
    animate(p_fw, renderer = file_renderer(dir = paste0("frames/", folder_name, "/Forward", l), prefix = "frame_", overwrite = TRUE), fps = 1, width = fig_width, height = fig_height, nframes = frame_num)
    animate(p_vert, renderer = file_renderer(dir = paste0("frames/", folder_name, "/Vertical", l), prefix = "frame_", overwrite = TRUE), fps = 1, width = fig_width, height = fig_height, nframes = frame_num)
    animate(p_side, renderer = file_renderer(dir = paste0("frames/", folder_name, "/Sideways", l), prefix = "frame_", overwrite = TRUE), fps = 1, width = fig_width, height = fig_height, nframes = frame_num)
  }
  
  #Now should move all of these to one folder in some sorten (padded) way. frame_##### format. 
  
  frame_counter_fw <- 0
  frame_counter_vert <- 0
  frame_counter_side <- 0
  
  for(l in 0:no_of_thousands){
    dir <- paste0("frames/", folder_name, "/Forward", l)
    output_dir <- paste0("frames/", folder_name, "/Forward")
    
    image_files <- list.files(dir, pattern = "\\.(jpg|jpeg|png|bmp|tiff)$", full.names = TRUE, ignore.case = TRUE)
    image_files <- sort(image_files)
    
    for(img in image_files){
      new_path <- file.path(output_dir, paste0("Frame", sprintf("%05d", frame_counter_fw), ".", tools::file_ext(img)))
      file.copy(img, new_path)
      frame_counter_fw <- frame_counter_fw + 1
    }
    
    dir <- paste0("frames/", folder_name, "/Vertical", l)
    output_dir <- paste0("frames/", folder_name, "/Vertical")
    
    image_files <- list.files(dir, pattern = "\\.(jpg|jpeg|png|bmp|tiff)$", full.names = TRUE, ignore.case = TRUE)
    image_files <- sort(image_files)
    
    for(img in image_files){
      new_path <- file.path(output_dir, paste0("Frame", sprintf("%05d", frame_counter_vert), ".", tools::file_ext(img)))
      file.copy(img, new_path)
      frame_counter_vert <- frame_counter_vert + 1
    }
    
    dir <- paste0("frames/", folder_name, "/Sideways", l)
    output_dir <- paste0("frames/", folder_name, "/Sideways")
    
    image_files <- list.files(dir, pattern = "\\.(jpg|jpeg|png|bmp|tiff)$", full.names = TRUE, ignore.case = TRUE)
    image_files <- sort(image_files)
    
    for(img in image_files){
      new_path <- file.path(output_dir, paste0("Frame", sprintf("%05d", frame_counter_side), ".", tools::file_ext(img)))
      file.copy(img, new_path)
      frame_counter_side <- frame_counter_side + 1
    }
  }
}

generate_server <- function(frame_tot,
                            folder_name,
                            streamvid,
                            stream1,
                            stream2,
                            stream3){
  
  
  
  ui <- fluidPage(
    titlePanel("Tool to compare acceleration data with video recordings"),
    
    fluidRow(
      column(3,
             sliderInput("frame", "Frame number:", min = 1, max = frame_tot, value = 1, step = 1)
      ),
      column(3,
             selectInput("frame_type", "Frame type:", 
                         choices = setNames(c("stream1", "stream2", "stream3"), c(stream1, stream2, stream3)),
                         selected = "stream2")
      ),
      column(3, 
             numericInput("delay", "Delay (s):", value = 1/30, min = 0, step = 1/90, width = "100px"))
    ),
    fluidRow(
      column(12,
             actionButton("prev", "Previous"),
             actionButton("next", "Next"),
             actionButton("play_pause", "Play")
      )
    ),
    fluidRow(
      column(7, imageOutput("frame_img", width = "100%", height = "320px")),
      column(5, imageOutput("video_img", width = "100%", height = "320px"))
    ),
    fluidRow(
      column(3, imageOutput("avg_img", width = "100%", height = "250px"))
      #Changing width does not seem to have an impact sadly
    )
  )
  
  server <- function(input, output, session) {
    frame_dir_stream1 <- paste0("frames/", folder_name, "/", stream1)
    frame_dir_stream2 <- paste0("frames/", folder_name, "/", stream2)
    frame_dir_stream3 <- paste0("frames/", folder_name, "/", stream3)
    frame_video_dir <- paste0("frames/", folder_name, "/", streamvid)
    
    frame_files_stream1 <- list.files(frame_dir_stream1, pattern = "\\.png$", full.names = TRUE)
    frame_files_stream1 <- sort(frame_files_stream1)
    frame_files_stream2 <- list.files(frame_dir_stream2, pattern = "\\.png$", full.names = TRUE)
    frame_files_stream2 <- sort(frame_files_stream2)
    frame_files_stream3 <- list.files(frame_dir_stream3, pattern = "\\.png$", full.names = TRUE)
    frame_files_stream3 <- sort(frame_files_stream3)
    
    video_files <- list.files(frame_video_dir, pattern = "\\.jpg$", full.names = TRUE)
    video_files <- sort(video_files)
    
    observe({
      updateSliderInput(session, "frame", max = length(frame_files_stream1))
    })
    
    observeEvent(input$`next`, {
      new_val <- min(input$frame + 1, length(frame_files_stream1))
      updateSliderInput(session, "frame", value = new_val)
    })
    
    observeEvent(input$prev, {
      new_val <- max(input$frame - 1, 1)
      updateSliderInput(session, "frame", value = new_val)
    })
    
    current_frame_files <- reactive({
      switch(input$frame_type,
             "stream1" = frame_files_stream1,
             "stream2" = frame_files_stream2,
             "stream3" = frame_files_stream3)
    })
    
    output$frame_img <- renderImage({
      files <- current_frame_files()
      list(src = files[input$frame], contentType = 'image/png')
    }, deleteFile = FALSE)
    
    
    output$video_img <- renderImage({
      list(src = video_files[input$frame], contentType = 'image/jpg')
    }, deleteFile = FALSE)
    
    output$avg_img <- renderImage({
      # Map the stream type to a filename
      stream_map <- list(
        stream1 = paste0("frames/", folder_name, "/Averages/avg_stream1.png"),
        stream2 = paste0("frames/", folder_name, "/Averages/avg_stream2.png"),
        stream3 = paste0("frames/", folder_name, "/Averages/avg_stream3.png")
      )
      
      file_path <- stream_map[[input$frame_type]]
      
      list(src = file_path, contentType = 'image/png', width = 800, height = 250)
    }, deleteFile = FALSE)
    
    
    
    playing <- reactiveVal(FALSE)
    
    observeEvent(input$play_pause, {
      playing(!playing())
      updateActionButton(session, "play_pause", label = if (playing()) "Pause" else "Play")
    })
    observe({
      if (playing()) {
        new_val <- if (input$frame < frame_tot) input$frame + 1 else 1
        updateSliderInput(session, "frame", value = new_val)
        Sys.sleep(input$delay)
      }
    })
  }
  
  return(list(ui, server))
}

generate_raw_images <- function(folder_name,
                                width,
                                start_time,
                                raw_df,
                                fix_y,
                                X_bottom,
                                X_top,
                                Y_bottom,
                                Y_top,
                                Z_bottom,
                                Z_top){
  vid_frames <- length(list.files(paste0("frames/", folder_name, "/raw"), pattern = "\\.jpg")) 
  end_time <- start_time - (1/30) + vid_frames/30 + (width - 1)*(1/30) 
  df <- filter(raw_df, time >= start_time & time <= end_time)
  
  temp <- df
  temp$time <- as.numeric(temp$time)
  
  half_width <- (width - 1)/2
  frame_tot <- vid_frames
  
  frame_list <- c()
  for(j in 1:frame_tot){
    frame_list <- append(frame_list, rep(j, width))
  }
  
  idx_list <- c()
  for(j in 1:frame_tot){
    idx_list <- append(idx_list, temp$time[j:(j + width - 1)])
  }
  
  val_list_X <- c()
  for(j in 1:frame_tot){
    val_list_X <- append(val_list_X, temp$X[ j:(j + width - 1)])
  }
  
  val_list_Y <- c()
  for(j in 1:frame_tot){
    val_list_Y <- append(val_list_Y, temp$Y[ j:(j + width - 1)])
  }
  
  val_list_Z <- c()
  for(j in 1:frame_tot){
    val_list_Z <- append(val_list_Z, temp$Z[ j:(j + width - 1)])
  }
  
  colour_list <- c()
  for(j in 1:frame_tot){
    colour_list <- append(colour_list, c(rep(0,half_width), 1, rep(0, half_width)))
  }
  
  new <- data.frame(frame = frame_list, 
                    idx = idx_list, 
                    X = val_list_X, 
                    Y = val_list_Y, 
                    Z = val_list_Z, 
                    colour = colour_list)
  
  p_X <- ggplot(new, aes(x = idx, y = X)) +
    geom_line() +
    geom_point(aes(color = factor(colour), size = factor(colour))) +
    scale_colour_manual(values = c("0" = "#377EB8", "1" = "black"),
                        labels = c("Context observations", "Current observation"),
                        name = "") +
    scale_size_manual(values = c("0" = 5, "1" = 15),
                      labels = c("Context observations", "Current observation"),
                      name = "") +
    transition_manual(frame) +
    ylim(X_bottom, X_top) +
    view_follow(fixed_y = fix_y) +
    labs(
      x = "Numerical time (seconds since Unix epoch)",
      y = TeX("Vertical acceleration in g (9.80665 m/s²)")
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 14),            
      axis.title = element_text(size = 14),      
      axis.text = element_text(size = 14),       
      legend.text = element_text(size = 14),     
      legend.title = element_text(size = 14)     
    )
  
  p_Y <- ggplot(new, aes(x = idx, y = Y)) +
    geom_line() +
    geom_point(aes(color = factor(colour), size = factor(colour))) +
    scale_colour_manual(values = c("0" = "#377EB8", "1" = "black"),
                        labels = c("Context observations", "Current observation"),
                        name = "") +
    scale_size_manual(values = c("0" = 5, "1" = 15),
                      labels = c("Context observations", "Current observation"),
                      name = "") +
    transition_manual(frame) +
    ylim(Y_bottom, Y_top) +
    view_follow(fixed_y = fix_y) +
    labs(
      x = "Numerical time (seconds since Unix epoch)",
      y = TeX("Vertical acceleration in g (9.80665 m/s²)")
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 14),            
      axis.title = element_text(size = 14),      
      axis.text = element_text(size = 14),       
      legend.text = element_text(size = 14),     
      legend.title = element_text(size = 14)     
    )
  
  p_Z <- ggplot(new, aes(x = idx, y = Z)) +
    geom_line() +
    geom_point(aes(color = factor(colour), size = factor(colour))) +
    scale_colour_manual(values = c("0" = "#377EB8", "1" = "black"),
                        labels = c("Context observations", "Current observation"),
                        name = "") +
    scale_size_manual(values = c("0" = 5, "1" = 15),
                      labels = c("Context observations", "Current observation"),
                      name = "") +
    transition_manual(frame) +
    ylim(Z_bottom, Z_top) +
    view_follow(fixed_y = fix_y) +
    labs(
      x = "Numerical time (seconds since Unix epoch)",
      y = TeX("Vertical acceleration in g (9.80665 m/s²)")
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 14),            
      axis.title = element_text(size = 14),      
      axis.text = element_text(size = 14),       
      legend.text = element_text(size = 14),     
      legend.title = element_text(size = 14)     
    )
  
  animate(p_X, renderer = file_renderer(dir = paste0("frames/", folder_name, "/X"), prefix = "frame_", overwrite = TRUE), fps = 1, width = fig_width, height = fig_height, nframes = frame_tot)
  animate(p_Y, renderer = file_renderer(dir = paste0("frames/", folder_name, "/Y"), prefix = "frame_", overwrite = TRUE), fps = 1, width = fig_width, height = fig_height, nframes = frame_tot)
  animate(p_Z, renderer = file_renderer(dir = paste0("frames/", folder_name, "/Z"), prefix = "frame_", overwrite = TRUE), fps = 1, width = fig_width, height = fig_height, nframes = frame_tot)
}