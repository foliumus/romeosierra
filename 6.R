# Load library
library(nortest)   # Untuk uji Anderson-Darling dan Lilliefors
library(ggplot2)   # Untuk visualisasi

# Set seed agar hasil dapat direproduksi
set.seed(123)

# Parameter Data 1
mu1 <- 110           # Mean
sigma1 <- 65         # Standard deviation

# Parameter Data 2
mu2 <- 100           # Mean
sigma2 <- 60         # Standard deviation

# Jumlah sampel
n <- 1000

# Generate data dari distribusi normal
data1 <- rnorm(n, mean = mu1, sd = sigma1)
data2 <- rnorm(n, mean = mu2, sd = sigma2)

# Ringkasan data
summary(data1)
summary(data2)

# ------------------ Uji Normalitas ------------------

# Kolmogorov-Smirnov (KS Test)
ks.test(data1, "pnorm", mean = mean(data1), sd = sd(data1))
ks.test(data2, "pnorm", mean = mean(data2), sd = sd(data2))

# Shapiro-Wilk Test
shapiro.test(data1)
shapiro.test(data2)

# Anderson-Darling Test
ad.test(data1)
ad.test(data2)

# Lilliefors Test
lillie.test(data1)
lillie.test(data2)

# ------------------ Visualisasi ------------------

# Histogram
hist(data1, col = "blue", breaks = 30, main = "Histogram Data1", 
     xlab = "Value", ylab = "Frequency")
hist(data2, col = "red", breaks = 30, main = "Histogram Data2", 
     xlab = "Value", ylab = "Frequency")

# QQ Plot
par(mfrow = c(1, 2))  # Layout berdampingan
qqnorm(data1, main = "QQ Plot Data1", col = "blue")
qqline(data1, col = "red", lwd = 2)

qqnorm(data2, main = "QQ Plot Data2", col = "blue")
qqline(data2, col = "red", lwd = 2)

par(mfrow = c(1, 1))  # Reset layout

# PP Plot Function
pp_plot <- function(data, title) {
  probs <- (1:length(data)) / (length(data) + 1)  # Empirical CDF
  theo_quantiles <- pnorm(sort(data), mean(data), sd(data))  # Theoretical CDF
  plot(theo_quantiles, probs, main = title,
       xlab = "Theoretical CDF", ylab = "Empirical CDF",
       col = "blue", pch = 19)
  abline(0, 1, col = "red", lwd = 2)
}

# PP Plot
par(mfrow = c(1, 2))  # Layout berdampingan
pp_plot(data1, "PP Plot Data1")
pp_plot(data2, "PP Plot Data2")
par(mfrow = c(1, 1))  # Reset layout
