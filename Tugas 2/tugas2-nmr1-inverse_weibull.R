# Set parameter Weibull
k <- 1.5
lambda <- 1
n <- 1000

# Inverse Transform Method (ITM) 
set.seed(123)  # Untuk hasil yang reproducible
U <- runif(n)  # Bilangan acak dari uniform(0,1)
X <- lambda * (-log(U))^(1/k)  # Transformasi Inverse Weibull

# Plot histogram
hist(X, breaks = 30, probability = TRUE, main = "Histogram & PDF Weibull",
     xlab = "x", col = "lightblue", border = "black")

# Kurva Weibull
x_vals <- seq(0, max(X), length.out = 100)
pdf_vals <- (k / lambda) * (x_vals / lambda)^(k - 1) * exp(-(x_vals / lambda)^k)
lines(x_vals, pdf_vals, col = "red", lwd = 2)
legend("topright", legend = c("Histogram", "PDF Weibull"), 
       col = c("lightblue", "red"), lwd = c(5, 2), bty = "n")
