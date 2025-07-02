#Regular sinusoid

n = 1000
noise = 0
df <- tibble(time = 1:n) %>%
  mutate(sin1 = sin(time/5) + rnorm(n, sd = noise))

ggplot(df[1:50,], aes(x = time, y = sin1)) + geom_point()

spectrum <- fft(df$sin1)
spectrum_magnitude <- Mod(spectrum)
plot(spectrum_magnitude, type = 'h', main = 'Frequency Spectrum', xlab = 'Frequency', ylab = 'Magnitude')

spectrum_magnitude <- Mod(spectrum)
top_20_indices <- order(spectrum_magnitude, decreasing = TRUE)[1:20]
top_20_values <- spectrum_magnitude[top_20_indices]
view(top_20_values)
view(top_20_indices)

##Correlation analysis
temp <- acf(df$sin1, lag.max = 100)
temp

#Sinusoid with noise

n = 1000
noise = 0.4
df <- tibble(time = 1:n) %>%
  mutate(sin1 = sin(time/5) + rnorm(n, sd = noise))

ggplot(df[1:50,], aes(x = time, y = sin1)) + geom_point()

spectrum <- fft(df$sin1)
spectrum_magnitude <- Mod(spectrum)
plot(spectrum_magnitude, type = 'h', main = 'Frequency Spectrum', xlab = 'Frequency', ylab = 'Magnitude')

spectrum_magnitude <- Mod(spectrum)
top_20_indices <- order(spectrum_magnitude, decreasing = TRUE)[1:20]
top_20_values <- spectrum_magnitude[top_20_indices]
view(top_20_values)
view(top_20_indices)

##Correlation analysis
acf(df$sin1, lag.max = 100)

#Sinusoid with skip of half a period

n = 1000
noise = 0
df <- tibble(time = 1:n) %>%
  mutate(sin1 = sin(time/5) + rnorm(n, sd = noise))
df$sin1[500:516] <- 0
df$sin1[517:1000] <- sin((500:983)/5)

ggplot(df[400:600,], aes(x = time, y = sin1)) + geom_point()

spectrum <- fft(df$sin1)
spectrum_magnitude <- Mod(spectrum)
plot(spectrum_magnitude, type = 'h', main = 'Frequency Spectrum', xlab = 'Frequency', ylab = 'Magnitude')

spectrum_magnitude <- Mod(spectrum)
top_20_indices <- order(spectrum_magnitude, decreasing = TRUE)[1:20]
top_20_values <- spectrum_magnitude[top_20_indices]
view(top_20_values)
view(top_20_indices)

##Correlation analysis
acf(df$sin1, lag.max = 100)

#Sinusoid with skip separately chosen from period

n = 1000
noise = 0
df <- tibble(time = 1:n) %>%
  mutate(sin1 = sin(time/5) + rnorm(n, sd = noise))
df$sin1[500:504] <- 0
df$sin1[505:1000] <- sin((500:995)/5)

ggplot(df[400:600,], aes(x = time, y = sin1)) + geom_point()

spectrum <- fft(df$sin1)
spectrum_magnitude <- Mod(spectrum)
plot(spectrum_magnitude, type = 'h', main = 'Frequency Spectrum', xlab = 'Frequency', ylab = 'Magnitude')

spectrum_magnitude <- Mod(spectrum)
top_20_indices <- order(spectrum_magnitude, decreasing = TRUE)[1:20]
top_20_values <- spectrum_magnitude[top_20_indices]
view(top_20_values)
view(top_20_indices)

##Correlation analysis
acf(df$sin1, lag.max = 100)

#Different short pauses at different locations and at different lengths
#Still no "underlying" seasonality throughout

n = 1000
noise = 0
df <- tibble(time = 1:n) %>%
  mutate(sin1 = sin(time/5) + rnorm(n, sd = noise))

df$sin1[100:104] <- 0
df$sin1[104:299] <- sin((100:295)/5)
df$sin1[300:343] <- 0
df$sin1[344:500] <- sin((296:452)/5)
df$sin1[501:510] <- 0
df$sin1[511:700] <- sin((453:642)/5)
df$sin1[701:725] <- 0
df$sin1[726:1000] <- sin((643:917)/5)

ggplot(df[650:760,], aes(x = time, y = sin1)) + geom_point()

spectrum <- fft(df$sin1)
spectrum_magnitude <- Mod(spectrum)
plot(spectrum_magnitude, type = 'h', main = 'Frequency Spectrum', xlab = 'Frequency', ylab = 'Magnitude')

spectrum_magnitude <- Mod(spectrum)
top_20_indices <- order(spectrum_magnitude, decreasing = TRUE)[1:20]
top_20_values <- spectrum_magnitude[top_20_indices]
view(top_20_values)
view(top_20_indices)

##Correlation analysis
temp <- acf(df$sin1, lag.max = 100)
temp
