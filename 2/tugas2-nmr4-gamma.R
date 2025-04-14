# Set parameter Gamma
k <- 3  # Shape parameter
lambda <- 2  # Rate parameter
n <- 1000  # Jumlah sampel

# PDF Gamma(3,2)
f_gamma <- function(x) {
  if (x > 0) {
    return((lambda^k * x^(k-1) * exp(-lambda * x)) / gamma(k))
  } else {
    return(0)
  }
}

# Distribusi pembantu: Eksponensial(2)
g_exp <- function(x) {
  return(lambda * exp(-lambda * x))  # PDF eksponensial
}

# Tentukan konstanta c (maksimum dari f(x)/g(x))
c <- (lambda^k / gamma(k)) * ((k-1)^(k-1) / exp(k-1))

# Acceptance-Rejection Method (ARM)
set.seed(123)
X <- numeric(n)
i <- 1

while (i <= n) {
  Y <- rexp(1, rate = lambda)  # Sampel dari Exp(2)
  U <- runif(1)  # Sampel acak untuk kriteria penerimaan
  if (U <= f_gamma(Y) / (c * g_exp(Y))) {  # Terima jika U < f(Y)/(c g(Y))
    X[i] <- Y
    i <- i + 1
  }
}

# Bandingkan dengan rgamma()
X_rgamma <- rgamma(n, shape = k, rate = lambda)

# Plot histogram
par(mfrow = c(1, 2))  # Bagi layar menjadi 2 grafik

# Histogram untuk ARM
hist(X, breaks = 30, probability = TRUE, main = "Histogram (ARM Gamma(3,2))",
     xlab = "x", col = "lightblue", border = "black")

# Histogram untuk rgamma()
hist(X_rgamma, breaks = 30, probability = TRUE, main = "Histogram (rgamma Gamma(3,2))",
     xlab = "x", col = "lightgreen", border = "black")

# Tambahkan kurva PDF Gamma(3,2)
x_vals <- seq(0, max(c(X, X_rgamma)), length.out = 100)
pdf_vals <- dgamma(x_vals, shape = k, rate = lambda)
