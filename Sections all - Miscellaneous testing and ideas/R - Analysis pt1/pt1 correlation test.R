library(tidyverse)

start_idx <- 271601
stop_idx <- 273883

df_epoch_raw %>% 
  filter(30*idx >= start_idx, idx*30 <= stop_idx) %>% 
  ggplot(aes(x = idx)) + 
  geom_line(aes(y = 300*Y_raw), colour = "red") +
  geom_line(aes(y = Y_epoch))

df_epoch_raw %>% 
  filter(30*idx >= start_idx, idx*30 <= stop_idx) %>%
  pull(Z_epoch) %>%
  max()

#Construction df_epoch_raw

temp <- df$vec_diff
result <- tapply(temp, (seq_along(temp) - 1) %/% 30, mean)

k = 41361
result[k]
mean(df$vec_diff[(30*k - 29):(30*k)])

#Computation activity values from ActiGraph
X_epoch = as.array(epoch_1$axis1)[1:543598]
Y_epoch = as.array(epoch_1$axis2)[1:543598]
Z_epoch = as.array(epoch_1$axis3)[1:543598]

#Computation 
temp_X <- df$X_diff[1:16307940]
X_raw = tapply(abs(temp_X), (seq_along(temp_X) - 1) %/% 30, mean)
temp_Y <- df$Y_diff[1:16307940]
Y_raw = tapply(abs(temp_Y), (seq_along(temp_Y) - 1) %/% 30, mean)
temp_Z <- df$Z_diff[1:16307940]
Z_raw = tapply(abs(temp_Z), (seq_along(temp_Z) - 1) %/% 30, mean)
temp_tot <- df$vec_diff[1:16307940]
tot_raw = tapply(abs(temp_tot), (seq_along(temp_tot) - 1) %/% 30, mean) #Here abs does not matter

cor(Z_epoch, Z_raw)

df_epoch_raw <- data.frame(X_raw = X_raw[1:543598], X_epoch = X_epoch[1:543598], Y_raw = Y_raw[1:543598], Y_epoch = Y_epoch[1:543598], Z_raw = Z_raw[1:543598], Z_epoch = Z_epoch[1:543598])
df_epoch_raw[187200:190800,] %>% ggplot() + geom_point(aes(x = Z_raw, y = Z_epoch))

df_epoch_raw <- mutate(df_epoch_raw, idx = 1:543598)

#Before 15-11-2024

#Trying different ways of measuring difference until correlation gets a lot higher

df_Z <- df$Z[1:16307940]
df_Z_shift <- df$Z[2:16307941]
temp_Z <- abs(df_Z - df_Z_shift)^(1/3)
temp_Z_raw = tapply(abs(temp_Z), (seq_along(temp_Z) - 1) %/% 30, mean)
cor(Z_epoch, temp_Z_raw)

#Seeing where differences are big

#epoch <- sqrt(epoch_X^2 + epoch_Y^2 + epoch_Z^2)
#cor(epoch[1:543598], tot_raw[1:543598])

df_epoch_raw <- data.frame(X_raw = X_raw[1:543598], X_epoch = X_epoch[1:543598], Y_raw = Y_raw[1:543598], Y_epoch = Y_epoch[1:543598], Z_raw = Z_raw[1:543598], Z_epoch = Z_epoch[1:543598])
df_epoch_raw[32000:35000,] %>% ggplot() + geom_point(aes(x = Z_raw, y = Z_epoch))

which.max(Z_epoch[32000:35000])
Z_epoch[34152]
Z_raw[34152]
k = 34152
view(df[(30*k - 29):(30*k),])
view(epoch_1[34152,])

### Below is old!

epoch <- sqrt(epoch_X^2 + epoch_Y^2 + epoch_Z^2)
cor(epoch[1:543598], result[1:543598])

#Maybe try taking square or different norm here or sth
epoch_alt <- epoch_X + epoch_Y + epoch_Z
epoch_alt <- (epoch_X^2 + epoch_Y^2 + epoch_Z^2)^(0.9)
cor(epoch_alt[1:543598], result[1:543598])

#X activity correlation
temp_X <- df$X_diff
result_X <- tapply(temp_X, (seq_along(temp_X) - 1) %/% 30, mean)
result_X_abs <- abs(result_X)

epoch_X <- as.array(epoch_1$axis1)

cor(epoch_X[1:543598], result_X_abs[1:543598])

#Y activity correlation
temp_Y <- df$Y_diff
result_Y <- tapply(temp_Y, (seq_along(temp_Y) - 1) %/% 30, mean)
result_Y_abs <- abs(result_Y)

epoch_Y <- as.array(epoch_1$axis2)

cor(epoch_Y[1:543598], result_Y_abs[1:543598])


#Z activity correlation
temp_Z <- df$Z_diff
result_Z <- tapply(temp_Z, (seq_along(temp_Z) - 1) %/% 30, mean)
result_Z_abs <- abs(result_Z)

epoch_Z <- as.array(epoch_1$axis3)^(0.55)

cor(epoch_Z[1:543598], result_Z_abs[1:543598])

#Trying to visualize correlation for z
df_epoch_raw <- data.frame(raw = result[1:543598], epoch = epoch[1:543598], X_raw = result_X_abs[1:543598], X_epoch = epoch_X[1:543598])
df_epoch_raw[32000:35000,] %>% ggplot() + geom_point(aes(x = raw, y = epoch))

### Firstly Martin's idea



### Next attempt: seeing where discrepancy highest

lm_act <- lm(formula = epoch ~ raw, data = df_epoch_raw)
coef(lm_act)

df_epoch_raw$epoch[34396]
df_epoch_raw$raw[34396]
k = 34396
view(df[(30*k - 59):(30*k+30),])

mean(df$vec_diff[(30*k - 29):(30*k)])
epoch[k]

