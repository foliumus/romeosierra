# Set seed untuk reproduksibilitas
set.seed(123)

# Parameter distribusi normal
mean_val <- 15
sd_val <- 5
n <- 1000

# 1. Metode dengan distribusi Uniform
uniform_samples <- replicate(n, mean(runif(12, min = -0.5, max = 0.5))) 
normal_uniform <- mean_val + sd_val * sqrt(12) * uniform_samples

# 2. Metode Box-Muller
u1 <- runif(n)
u2 <- runif(n)
z1 <- sqrt(-2 * log(u1)) * cos(2 * pi * u2)
normal_box_muller <- mean_val + sd_val * z1

# 3. Menggunakan fungsi rnorm()
normal_rnorm <- rnorm(n, mean = mean_val, sd = sd_val)

# Membuat Histogram dan Density Plot
par(mfrow = c(1,3))  

# Plot untuk metode Uniform
hist(normal_uniform, breaks = 30, probability = TRUE, main = "Metode Uniform", col = "skyblue", border = "black")
lines(density(normal_uniform), col = "red", lwd = 2)

# Plot untuk metode Box-Muller
hist(normal_box_muller, breaks = 30, probability = TRUE, main = "Metode Box-Muller", col = "lightgreen", border = "black")
lines(density(normal_box_muller), col = "red", lwd = 2)

# Plot untuk metode rnorm()
hist(normal_rnorm, breaks = 30, probability = TRUE, main = "Metode rnorm()", col = "lightcoral", border = "black")
lines(density(normal_rnorm), col = "red", lwd = 2)


