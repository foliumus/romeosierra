library(ggplot2)

# Memastikan reproducibility
set.seed(123)

# Pembuatan fungsi distribusi F
fdistt <- function (df1, df2, n){
  x1 <- rchisq(n, df1)
  x2 <- rchisq(n, df2)
  f <- (x1/df1)/(x2/df2)
  return(f)
}

# Deklarasi parameter dan jumlah data
fdat1 <- fdistt(5, 10, 1000) #df1 = 5, df2= 10
fdat2 <- fdistt(10, 20, 1000) #df1 = 10, df2= 20
fdat3 <- fdistt(20, 40, 1000) #df1 = 20, df2= 40

# Pembuatan histogram
# df = 5 dan df = 10
ggplot(data = data.frame(fdat1), aes(x = fdat1)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "black", color = "white", alpha = 0.7) +
  stat_function(fun = dt, args = list(df = 10), color = "violet", size = 1) + 
  labs(title = "Histogram dari distribusi F dengan df1 = 5 dan df2 = 10", x = "nilai t", y = "Density") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

# df = 10 dan df = 20
ggplot(data = data.frame(fdat2), aes(x = fdat2)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "black", color = "white", alpha = 0.7) +
  stat_function(fun = dt, args = list(df = 10), color = "violet", size = 1) + 
  labs(title = "Histogram dari distribusi F dengan df1 = 10 dan df2 = 20", x = "nilai t", y = "Density") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

# df = 20 dan df = 40
ggplot(data = data.frame(fdat3), aes(x = fdat3)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "black", color = "white", alpha = 0.7) +
  stat_function(fun = dt, args = list(df = 10), color = "violet", size = 1) + 
  labs(title = "Histogram dari distribusi F dengan df1 = 20 dan df2 = 40", x = "nilai t", y = "Density") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
