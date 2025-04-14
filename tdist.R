library(ggplot2)

# Memastikan reproducibility
set.seed(123)

# Pembuatan fungsi distribusi t
tdist <- function(df, n){
  Z <- rnorm(n)
  X <- rchisq(n, df)
  T <- Z / sqrt (X/df)
  return(T)
}

# Deklarasi parameter dan jumlah data
tdf5 <- tdist(5, 1000) # df = 5
tdf10 <- tdist(10, 1000) # df = 10
tdf30 <- tdist(30, 1000) # df = 30


# Pembuatan histogram
# df=5
ggplot(data = data.frame(tdf5), aes(x = tdf5)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "black", color = "white", alpha = 0.7) +
  stat_function(fun = dt, args = list(df = 10), color = "violet", size = 1) + 
  labs(title = "Histogram dari distribusi t dengan df = 5", x = "nilai t", y = "Density") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

# df=10
ggplot(data = data.frame(tdf10), aes(x = tdf10)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "black", color = "white", alpha = 0.7) +
  stat_function(fun = dt, args = list(df = 10), color = "violet", size = 1) + 
  labs(title = "Histogram dari distribusi t dengan df = 10", x = "nilai t", y = "Density") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

# df = 30
ggplot(data = data.frame(tdf30), aes(x = tdf30)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "black", color = "white", alpha = 0.7) +
  stat_function(fun = dt, args = list(df = 10), color = "violet", size = 1) + 
  labs(title = "Histogram dari distribusi t dengan df = 30", x = "nilai t", y = "Density") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))