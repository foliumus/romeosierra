library(nortest)
library(ggplot2)
# Set seed 
set.seed(123)

# Parameter untuk Data 1
mu1 <- 60 + 50  # Mean
sigma1 <- 15 + 50  # Standard deviation

# Parameter untuk Data 2
mu2 <- 50 + 50  # Mean
sigma2 <- 10 + 50  # Standard deviation

# Jumlah sampel
n <- 1000 

# Generate data dengan distribusi normal
data1 <- rnorm(n, mean = mu1, sd = sigma1)
data2 <- rnorm(n, mean = mu2, sd = sigma2)

# Summary
summary(data1)
summary(data2)

data1 <- data.frame(data1)
data2 <- data.frame(data2)
# Normalitas
# KS
ks.test(data1, "pnorm", mean = mean(data1), sd = sd(data1))
ks.test(data2,  "pnorm", mean = mean(data2), sd = sd(data2))

# SW
shapiro.test(data1)
shapiro.test(data2)

# AD
ad.test(data1)
ad.test(data2)

# Lillie
lillie.test(data1)
lillie.test(data2)\

# Histogram
# Set layout untuk menampilkan 2 histogram berdampingan

par(mfrow = c(1, 2))  

# Histogram Data1
hist(data1, col = "blue", breaks = 30, main = "Histogram Data1", 
     xlab = "Value", ylab = "Frequency")

# Histogram Data2
hist(data2, col = "red", breaks = 30, main = "Histogram Data2", 
     xlab = "Value", ylab = "Frequency")

# Reset layout
par(mfrow = c(1, 1))

# Set layout untuk menampilkan 2 QQ Plot berdampingan
par(mfrow = c(1, 2))  

# QQ Plot untuk Data1
qqnorm(data1, main = "QQ Plot Data1", col = "blue")
qqline(data1, col = "red", lwd = 2)  # Tambahkan garis referensi

# QQ Plot untuk Data2
qqnorm(data2, main = "QQ Plot Data2", col = "blue")
qqline(data2, col = "red", lwd = 2)  # Tambahkan garis referensi

# Reset layout ke default
par(mfrow = c(1, 1))

# Fungsi untuk membuat PP Plot
pp_plot <- function(data, title) {
  probs <- (1:length(data)) / (length(data) + 1)  # Probabilitas empiris
  theo_quantiles <- pnorm(sort(data), mean(data), sd(data))  # Probabilitas teoretis
  
  plot(theo_quantiles, probs, main = title, xlab = "Theoretical CDF", 
       ylab = "Empirical CDF", col = "blue", pch = 19)
  abline(0, 1, col = "red", lwd = 2)  # Garis referensi
}

# Set layout untuk 2 plot berdampingan
par(mfrow = c(1, 2))

# PP Plot untuk Data1
pp_plot(data1, "PP Plot Data1")

# PP Plot untuk Data2
pp_plot(data2, "PP Plot Data2")

# Reset layout ke default
par(mfrow = c(1, 1))


