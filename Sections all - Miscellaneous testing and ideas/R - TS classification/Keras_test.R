library(tidyverse)
#library(keras)

#remove.packages("keras")
#install.packages("keras3") 

library(keras3) 

#16-10: Try the same thing that was done in the ADS2 course!:


model <- keras_model_sequential(input_shape = c(3)) %>%
  layer_dense(units = 10, activation = "relu") %>% # Hidden layer with 10 units
  layer_dense(units = 1, activation = "sigmoid")  # Output layer with 1 unit for binary classificatio

model$compile(
  loss = "binary_crossentropy",
  optimizer = "adam",
  metrics = list("accuracy")
)

# Create the example dataframe
dfx <- data.frame(
  col1 = c(1, 2, 3, 4, 5, 6, 7, 8,0,1,2,3,4,5,6,7,8),
  col2 = c(0, 1, 2, 3, 4, 5, 6, 7,3,4,1,6,3,4,5,6,7),
  col3 = c(4, 3, 1, 1, 5, 3, 2, 1,4,3,6,2,5,8,6,4,2)
)

dfx <- as.matrix(dfx)

#Should be 1 if sum is at least 10
dfy <- as.matrix(data.frame(y = c(0,0,0,0,1,1,1,1,0,0,0,1,1,1,1,1,1,1)))

model %>% fit(x = dfx, y = dfy, epochs = 500, validation_split = 0.2, verbose = 1)

# Separate features and target
x_train <- as.matrix(df[, c("col1", "col2", "col3")])
y_train <- df$y

# Convert labels to categorical
y_train <- matrix(0, nrow = length(y_train), ncol = 2)
y_train[cbind(1:length(df$y), df$y + 1)] <- 1

#Skip below for now

LSTM_mod <- keras_model_sequential() %>%
  layer_dense(units = 10, activation = "softmax", input_shape = c(8)) 

LSTM_mod <- keras_model_sequential() %>%
  layer_input(shape = c(8)) %>%  # Explicitly define the input layer
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 10, activation = "softmax")

LSTM_mod <- keras_model_sequential(input_shape = c(8)) %>%
  layer_dense(10, activation = "softmax")

test = as.array(c(9,9,9)) %>% t()
predict(model, x = test)

#We will now attempt the same, but with our sinusoid data
n = 1000
df <- tibble(time = 1:n) %>%
  mutate(sin1 = sin(time/5) + rnorm(n, sd = 0.1)) %>%
  mutate(sin2 = sin(time/5) + rnorm(n, sd = 0.1)) %>%
  mutate(sin3 = sin(time/5) + rnorm(n, sd = 0.1)) %>%
  mutate(unif1 = sin(runif(n, 0, 2*pi)) + rnorm(n, sd = 0.1)) %>%
  mutate(unif2 = sin(runif(n, 0, 2*pi)) + rnorm(n, sd = 0.1)) %>%
  mutate(unif3 = sin(runif(n, 0, 2*pi)) + rnorm(n, sd = 0.1))  

view(df)

#And now the same, but more cleanly

time = 1:n

x_array <- as.array(sin(time/5) + rnorm(n, sd = 0.1)) %>% t()

for(t in 1:10000){
  temp <- as.array(sin(runif(n, 0, 2*pi)) + rnorm(n, sd = 0.1)) %>% t()
  x_array <- x_array %>% rbind(temp)
}

for(t in 1:9999){
  temp <- as.array(sin(time/5) + rnorm(n, sd = 0.1)) %>% t()
  x_array <- x_array %>% rbind(temp)
}

y_array <-  as.array(c(1, rep(0,10000), rep(1,9999)))

#So first n columns will be the x-data, last one will be the label. We first shuffle the rows

model <- keras_model_sequential(input_shape = c(n)) %>% 
  layer_dense(128, activation = "relu") %>% 
  layer_dense(64, activation = "relu") %>% 
  layer_dense(1, activation = "sigmoid")

model$compile(
  loss = "binary_crossentropy",
  optimizer = "adam",
  metrics = list("accuracy")
)

model %>% fit(x = x_array, y = y_array, epochs = 5, validation_split = 0.2, verbose = 1)

test_sin <- as.array(sin(time/5) + rnorm(n, sd = 0.1)) %>% t()
predict(model, test_sin)

test_unif <- as.array(sin(runif(n, 0, 2*pi)) + rnorm(n, sd = 0.1)) %>% t()
predict(model, test_unif)

#Below if I want to shuffle later a start, but not really necessary

#We will try to reproduce this, but shuffle it all. Order matters (although I am unsure why it would indicate low val_loss)
#Can probably think of a better alternative

n = 10
time = 1:n

x_array <- as.array(sin(time/5) + rnorm(n, sd = 0.1)) %>% append(1)  %>% t()
dim(x_array)
temp <- as.array(sin(runif(n, 0, 2*pi)) + rnorm(n, sd = 0.1)) %>% append(1) %>% t()
dim(temp)
x_array <- sinusoid_array %>% rbind(temp)


for(t in 1:10000){
  temp <- as.array(sin(runif(n, 0, 2*pi)) + rnorm(n, sd = 0.1)) %>% append(1) %>% t()
  x_array <- sinusoid_array %>% rbind(temp)
}

for(t in 1:9999){
  temp <- as.array(sin(time/5) + rnorm(n, sd = 0.1)) %>% append(0) %>% t()
  x_array <- sinusoid_array %>% rbind(temp)
}
