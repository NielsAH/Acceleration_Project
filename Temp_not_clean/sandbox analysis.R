#Example: all data of S1. Firstly, we take "percentage of acceleration that is forward"
#We consider all steps in a movement sequence except the first and last one

df_analysis <- data.frame(individual = c("S1", "S2", "S3", "S4", "S5", "H10", "H11", "H12", "H13", "H14", "A20", "A21", "walk1", "walk2", "walk3", "walk4", "walk5", "walk6", "walk7", "walk8", "walk9", "walk10"))
df_analysis$type <- c(rep("S", 5), rep("H", 5), rep("A", 2), rep("walk", 10))

S1_list <- list(seq_S1)
S2_list <- list(seq_S2)
S3_list <- list(seq_S3)
S4_list <- list(seq_S4)
S5_list <- list(seq_S5)
S6_list <- list()
S7_list <- list()
S8_list <- list()
S9_list <- list()
H10_list <- list(seq_H10)
H11_list <- list(seq_H11)
H12_list <- list(seq_H12)
H13_list <- list(seq_H13)
H14_list <- list(seq_H14)
H15_list <- list()
H16_list <- list()
H17_list <- list()
H18_list <- list()
H19_list <- list()

A20_list <- list(seq_A20_1, seq_A20_2)
A21_list <- list(seq_A21)

walk1_list <- list(seq_walk1)
walk2_list <- list(seq_walk2)
walk3_list <- list(seq_walk3)
walk4_list <- list(seq_walk4)
walk5_list <- list(seq_walk5)
walk6_list <- list(seq_walk6)
walk7_list <- list(seq_walk7)
walk8_list <- list(seq_walk8)
walk9_list <- list(seq_walk9)
walk10_list <- list(seq_walk10)

#First everything per step

#First num and efficiency: percentage total acceleration that is forward
step_num <- c()
efficiency_mean <- c()
efficiency_5 <- c()
efficiency_95 <- c()
  
for(idv in df_analysis$individual){
  
  steps_data <- c()
  
  sequence_lists <- get(paste0(idv, "_list"))
  print(idv)
  
  
  for(seq_list in sequence_lists){
    
    for(seq in seq_list){
      if(seq$movement_type == "Walk"){
        
        end_bw <- max(seq$data$in_step_bw)
        end_fw <- max(seq$data$in_step_fw)
        
        if(end_bw > 1){
          
          for(j in 1:(end_bw - 1)){
            
            step <- filter(seq$data, in_step_bw == j)
            
            fraction_fw <- sum(abs(step$acc_fw))/(sum(abs(step$acc_fw)) + sum(abs(step$acc_side)) + sum(abs(step$acc_vert)))
            
            data <- fraction_fw
            
            steps_data <- append(steps_data, data)
            
          }
          
        }
        
        if(end_fw > 1){
          
          for(j in 1:(end_fw - 1)){
            
            step <- filter(seq$data, in_step_fw == j)
            
            fraction_fw <- sum(abs(step$acc_fw))/(sum(abs(step$acc_fw)) + sum(abs(step$acc_side)) + sum(abs(step$acc_vert)))
            
            data <- fraction_fw
            
            steps_data <- append(steps_data, data)
          }
        }
      }
    }
  }
  
  if(length(steps_data) == 0){
    step_num <- append(step_num, 0)
    efficiency_mean <- append(efficiency_mean, NA)
    efficiency_5 <- append(efficiency_5, NA)
    efficiency_95 <- append(efficiency_95, NA)
  }
  else{
    step_num <- append(step_num, length(steps_data))
    efficiency_mean <- append(efficiency_mean, mean(steps_data))
    efficiency_5 <- append(efficiency_5, quantile(steps_data, probs = c(0.05, 0.95))[[1]])
    efficiency_95 <- append(efficiency_95, quantile(steps_data, probs = c(0.05, 0.95))[[2]])
  }
}

df_analysis$num <- step_num
df_analysis$efficiency_mean <- efficiency_mean
df_analysis$efficiency_5 <- efficiency_5
df_analysis$efficiency_95 <- efficiency_95

#Adding more

ggplot(df_analysis, aes(color = type)) + 
  geom_point(aes(x = efficiency_5, y = efficiency_95)) +
  theme_minimal()
