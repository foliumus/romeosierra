# Set parameter Weibull
n <- 1000
k <- 1.5  # Shape parameter
lambda <- 1  # Scale parameter

# Metode Transformasi Langsung
set.seed(123)
U <- runif(n)  # Bilangan acak dari Uniform(0,1)
X_weibull <- lambda * (-log(U))^(1/k)  # Transformasi langsung

# Bandingkan dengan rweibull()
X_rweibull <- rweibull(n, shape = k, scale = lambda)

# Plot histogram
par(mfrow = c(1, 2))  # Bagi layar menjadi 2 grafik

# Histogram untuk Metode Transformasi Langsung
hist(X_weibull, breaks = 30, probability = TRUE, main = "Histogram (Transformasi Langsung)",
     xlab = "x", col = "lightblue", border = "black")

# Histogram untuk rweibull()
hist(X_rweibull, breaks = 30, probability = TRUE, main = "Histogram (rweibull())",
     xlab = "x", col = "lightgreen", border = "black")

# Tambahkan kurva PDF Weibull
x_vals <- seq(0, max(c(X_weibull, X_rweibull)), length.out = 100)
pdf_vals <- dweibull(x_vals, shape = k, scale = lambda)
