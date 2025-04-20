generate_data <- function(folder_name_vid_frames,
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
  
  vid_frames <- length(list.files(folder_name_vid_frames, pattern = "\\.jpg")) 
  
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

generate_server <- function(vid_name,
                            folder_name_vid_frames,
                            folder_name_frames,
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
  
  vid_frames <- length(list.files(folder_name_vid_frames, pattern = "\\.jpg")) 
  
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
    else if((df$in_step_bw[idx] %% 2 == 1) | (df$in_step_fw[idx] %% 2 == 1)){
      return(3)
    }
    else{
      return(4)
    }
  }
  
  alt_colour_list <- c()
  for(j in 1:frame_tot){
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
  
  p_fw <- ggplot(new, aes(x = idx, y = value_fw)) +
    geom_line() +
    geom_point(aes(color = factor(colour), size = factor(colour))) +
    scale_colour_manual(values = c("1" = "black", "2" = "#984EA3", "3" = "#009E73", "4" = "#E69F00"),
                        labels = c("Current observation", "No activity", "Odd step pairs", "Even step pairs"),
                        name = "") +
    scale_size_manual(values = c("1" = 15, "2" = 5, "3" = 5, "4" = 5),
                      labels = c("Current observation", "No activity", "Odd step pairs", "Even step pairs"),
                      name = "") +
    transition_states(frame, transition_length = 0, state_length = 1) +
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
  
  p_vert <- ggplot(new, aes(x = idx, y = value_vert)) +
    geom_line() +
    geom_point(aes(color = factor(colour), size = factor(colour))) +
    scale_colour_manual(values = c("1" = "black", "2" = "#984EA3", "3" = "#009E73", "4" = "#E69F00"),
                        labels = c("Current observation", "No activity", "Odd step pairs", "Even step pairs"),
                        name = "") +
    scale_size_manual(values = c("1" = 15, "2" = 5, "3" = 5, "4" = 5),
                      labels = c("Current observation", "No activity", "Odd step pairs", "Even step pairs"),
                      name = "") +
    transition_states(frame, transition_length = 0, state_length = 1) +
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
  
  p_side <- ggplot(new, aes(x = idx, y = value_side)) +
    geom_line() +
    geom_point(aes(color = factor(colour), size = factor(colour))) +
    scale_colour_manual(values = c("1" = "black", "2" = "#984EA3", "3" = "#009E73", "4" = "#E69F00"),
                        labels = c("Current observation", "No activity", "Odd step pairs", "Even step pairs"),
                        name = "") +
    scale_size_manual(values = c("1" = 15, "2" = 5, "3" = 5, "4" = 5),
                      labels = c("Current observation", "No activity", "Odd step pairs", "Even step pairs"),
                      name = "") +
    transition_states(frame, transition_length = 0, state_length = 1) +
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
  
  animate(p_fw, renderer = file_renderer(dir = paste0(folder_name_frames, "_fw"), prefix = "frame_", overwrite = TRUE), fps = 1, width = fig_width, height = fig_height, nframes = frame_tot)
  animate(p_vert, renderer = file_renderer(dir = paste0(folder_name_frames, "_vert"), prefix = "frame_", overwrite = TRUE), fps = 1, width = fig_width, height = fig_height, nframes = frame_tot)
  animate(p_side, renderer = file_renderer(dir = paste0(folder_name_frames, "_side"), prefix = "frame_", overwrite = TRUE), fps = 1, width = fig_width, height = fig_height, nframes = frame_tot)
  
  ui <- fluidPage(
    titlePanel("Tool to compare acceleration data with video recordings"),
    
    fluidRow(
      column(3,
             sliderInput("frame", "Frame number:", min = 1, max = frame_tot, value = 1, step = 1)
      ),
      column(3,
             selectInput("frame_type", "Frame type:", 
                         choices = c("Forward" = "fw", "Vertical" = "vert", "Side" = "side"),
                         selected = "fw")
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
      column(7, imageOutput("frame_img", width = "10%", height = "10px")),
      column(5, imageOutput("video_img", width = "10%", height = "10px"))
    )
  )
  
  server <- function(input, output, session) {
    frame_dir_fw <- paste0(folder_name_frames, "_fw")
    frame_dir_vert <- paste0(folder_name_frames, "_vert")
    frame_dir_side <- paste0(folder_name_frames, "_side")
    frame_video_dir <- folder_name_vid_frames
    
    frame_files_fw <- list.files(frame_dir_fw, pattern = "\\.png$", full.names = TRUE)
    frame_files_fw <- sort(frame_files_fw)
    frame_files_vert <- list.files(frame_dir_vert, pattern = "\\.png$", full.names = TRUE)
    frame_files_vert <- sort(frame_files_vert)
    frame_files_side <- list.files(frame_dir_side, pattern = "\\.png$", full.names = TRUE)
    frame_files_side <- sort(frame_files_side)
    
    video_files <- list.files(frame_video_dir, pattern = "\\.jpg$", full.names = TRUE)
    video_files <- sort(video_files)
    
    observe({
      updateSliderInput(session, "frame", max = length(frame_files_fw))
    })
    
    observeEvent(input$`next`, {
      new_val <- min(input$frame + 1, length(frame_files_fw))
      updateSliderInput(session, "frame", value = new_val)
    })
    
    observeEvent(input$prev, {
      new_val <- max(input$frame - 1, 1)
      updateSliderInput(session, "frame", value = new_val)
    })
    
    current_frame_files <- reactive({
      switch(input$frame_type,
             "fw" = frame_files_fw,
             "vert" = frame_files_vert,
             "side" = frame_files_side)
    })
    
    output$frame_img <- renderImage({
      files <- current_frame_files()
      list(src = files[input$frame], contentType = 'image/png')
    }, deleteFile = FALSE)
    
    
    output$video_img <- renderImage({
      list(src = video_files[input$frame], contentType = 'image/jpg')
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

generate_raw_server <- function(
                            folder_name_vid_frames,
                            folder_name_frames,
                            width,
                            ylim_bottom,
                            ylim_top,
                            fig_width,
                            fig_height,
                            fix_y,
                            df_acc_raw){
  
  
  temp <- df_acc_raw
  temp$time <- as.numeric(temp$time)
  
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
  
  val_list <- c()
  for(j in 1:frame_tot){
    val_list <- append(val_list, temp$Y[ j:(j + width - 1)])
  }
  
  colour_list <- c()
  for(j in 1:frame_tot){
    colour_list <- append(colour_list, c(rep(0, half_width), 1, rep(0, half_width)))
  }
  
  new <- data.frame(frame = frame_list, idx = idx_list, value = val_list, colour = colour_list)
  
  if(fix_y == T){
    p <- ggplot(new, aes(x = idx, y = value)) +
      geom_line() +
      geom_point(aes(color = factor(colour), size = factor(colour))) +
      scale_colour_manual(values = c("0" = "#984EA3", "1" = "#009E73")) +
      scale_size_manual(values = c("0" = 5, "1" = 15, "2" = 5, "3" = 5, "4" = 5)) +
      transition_states(frame, transition_length = 0, state_length = 1) +
      ylim(ylim_bottom, ylim_top) +
      view_follow(fixed_y = T)  
  } else{
    p <- ggplot(new, aes(x = idx, y = value)) +
      geom_line() +
      geom_point(aes(color = factor(colour), size = factor(colour))) +
      scale_colour_manual(values = c("0" = "#984EA3", "1" = "#009E73")) +
      scale_size_manual(values = c("0" = 5, "1" = 15, "2" = 5, "3" = 5, "4" = 5)) +
      transition_states(frame, transition_length = 0, state_length = 1) +
      ylim(ylim_bottom, ylim_top) +
      view_follow(fixed_y = T)  
  }
  
  animate(p, renderer = file_renderer(dir = folder_name_frames, prefix = "frame_", overwrite = TRUE), fps = 1, width = fig_width, height = fig_height, nframes = frame_tot)
  
  ui <- fluidPage(
    titlePanel("Frame Viewer"),
    sliderInput("frame", "Frame:", min = 1, max = frame_tot, value = 1, step = 1),
    fluidRow(
      column(10,
             actionButton("prev", "Previous"),
             actionButton("next", "Next"),
             actionButton("play_pause", "Play"),
             numericInput("delay", "Delay (s):", value = 1/30, min = 0, step = 1/90, width = "100px")
      )
    ),
    fluidRow(
      column(7, imageOutput("frame_img", width = "10%", height = "10px")),
      column(5, imageOutput("video_img", width = "10%", height = "10px"))
    )
  )
  
  server <- function(input, output, session) {
    frame_dir <- folder_name_frames
    frame_video_dir <- folder_name_vid_frames
    
    frame_files <- list.files(frame_dir, pattern = "\\.png$", full.names = TRUE)
    frame_files <- sort(frame_files)
    
    video_files <- list.files(frame_video_dir, pattern = "\\.jpg$", full.names = TRUE)
    video_files <- sort(video_files)
    
    observe({
      updateSliderInput(session, "frame", max = length(frame_files))
    })
    
    observeEvent(input$`next`, {
      new_val <- min(input$frame + 1, length(frame_files))
      updateSliderInput(session, "frame", value = new_val)
    })
    
    observeEvent(input$prev, {
      new_val <- max(input$frame - 1, 1)
      updateSliderInput(session, "frame", value = new_val)
    })
    
    output$frame_img <- renderImage({
      list(src = frame_files[input$frame], contentType = 'image/png')
    }, deleteFile = FALSE)
    
    
    output$video_img <- renderImage({
      list(src = video_files[input$frame], contentType = 'image/jpg')
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
