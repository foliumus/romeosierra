# Set parameter Log-Normal
mu <- 0
sigma <- 1
n <- 1000

# Inverse Transform Method (ITM)
set.seed(123)  # Untuk hasil yang sama setiap kali dijalankan
U <- runif(n)  # Bilangan acak dari uniform(0,1)
Z <- qnorm(U, mean = 0, sd = 1)  # Transformasi ke normal standar
X <- exp(mu + sigma * Z)  # Transformasi ke log-normal

# Plot histogram
hist(X, breaks = 30, probability = TRUE, main = "Histogram & PDF Log-Normal",
     xlab = "x", col = "lightblue", border = "black")

# Tambahkan kurva PDF Log-Normal
x_vals <- seq(0, max(X), length.out = 100)
pdf_vals <- dlnorm(x_vals, meanlog = mu, sdlog = sigma)
lines(x_vals, pdf_vals, col = "red", lwd = 2)

# Tambahkan legenda
legend("topright", legend = c("Histogram", "PDF Log-Normal"), 
       col = c("lightblue", "red"), lwd = c(5, 2), bty = "n")
