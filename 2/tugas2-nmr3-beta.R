# Set jumlah sampel
n <- 1000

# PDF Beta(2,2)
f_beta <- function(x) {
  return(6 * x * (1 - x))  # f(x) = 6x(1-x) untuk 0 < x < 1
}

# Gunakan Uniform(0,1) sebagai proposal g(x), dengan konstanta c=1.5
c <- 1.5  # Nilai maksimum dari f(x)/g(x)

# Acceptance-Rejection Method (ARM)
set.seed(123)
X <- numeric(n)  # Simpan hasil sampel
i <- 1  # Counter

while (i <= n) {
  x_proposed <- runif(1)  # Sampel dari distribusi pembantu g(x) ~ U(0,1)
  u <- runif(1)  # Sampel acak untuk kriteria penerimaan
  if (u <= f_beta(x_proposed) / (c * 1)) {  # Terima jika u < f(x)/cg(x)
    X[i] <- x_proposed
    i <- i + 1
  }
}

# Bandingkan dengan rbeta()
X_rbeta <- rbeta(n, shape1 = 2, shape2 = 2)

# Plot histogram
par(mfrow = c(1, 2))  # Bagi layar menjadi 2 grafik

# Histogram untuk ARM
hist(X, breaks = 30, probability = TRUE, main = "Histogram (ARM Beta(2,2))",
     xlab = "x", col = "lightblue", border = "black")

# Histogram untuk rbeta()
hist(X_rbeta, breaks = 30, probability = TRUE, main = "Histogram (rbeta Beta(2,2))",
     xlab = "x", col = "lightgreen", border = "black")

# Tambahkan kurva PDF Beta(2,2)
x_vals <- seq(0, 1, length.out = 100)
pdf_vals <- dbeta(x_vals, shape1 = 2, shape2 = 2)