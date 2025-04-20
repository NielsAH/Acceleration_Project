library(tidyverse)
library(nleqslv)

df <- A20

start_times <- c(as.POSIXct("2024-10-11 02:00:00.000"),
                 as.POSIXct("2024-10-12 11:00:00.000"),
                 as.POSIXct("2024-10-13 02:00:00.000"),
                 as.POSIXct("2024-10-12 02:00:00.000")
)
end_times  <- c(as.POSIXct("2024-10-11 03:00:00.000"),
                as.POSIXct("2024-10-12 11:05:00.000"),
                as.POSIXct("2024-10-13 03:00:00.000"),
                as.POSIXct("2024-10-12 03:00:00.000")
)


four_starts <- start_times
four_ends <- end_times

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

vec1 <- c(0.004, 0.004, 1)
vec2 <- c(0.004, 1, 0.004)
vec3 <- c(1, 0.004, 0.004)
vec4 <- c(0.584, 0.58, 0.576)

(distances <- as.matrix(dist(rbind(vec1, vec2, vec3, vec4))))
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
