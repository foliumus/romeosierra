# Set jumlah sampel dan derajat kebebasan
n <- 1000
k <- 4  # Derajat kebebasan Chi-Square

# Metode Transformasi Langsung
set.seed(123)
Z <- matrix(rnorm(n * k, mean = 0, sd = 1), nrow = n, ncol = k)  # Matriks (1000x4) dari N(0,1)
X_chisq <- rowSums(Z^2)  # Jumlah kuadrat setiap baris

# Bandingkan dengan rchisq()
X_rchisq <- rchisq(n, df = k)

# Plot histogram
par(mfrow = c(1, 2))  # Bagi layar menjadi 2 grafik

# Histogram untuk Metode Transformasi Langsung
hist(X_chisq, breaks = 30, probability = TRUE, main = "Histogram (Transformasi Langsung)",
     xlab = "x", col = "lightblue", border = "black")

# Histogram untuk rchisq()
hist(X_rchisq, breaks = 30, probability = TRUE, main = "Histogram (rchisq())",
     xlab = "x", col = "lightgreen", border = "black")

# Tambahkan kurva PDF Chi-Square
x_vals <- seq(0, max(c(X_chisq, X_rchisq)), length.out = 100)
pdf_vals <- dchisq(x_vals, df = k)




