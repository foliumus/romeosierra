#Bangkitkan bilangan acak yang berdistribusi eksponensial(3)
 dengan Inverse Tranform Method, yang amatannya berjumlah 1000.
 bandingkan hasilnya dengan fungsi bawaan R rexp dengan
 menggunakanhistogram(Buatkan Langkah danalgoritmanya)

set.seed(123)
n <- 1000
lambda <- 3
unif <- runif(n)
inv <- -log(pmax(1 - unif)) / lambda
exp <- rexp(n, rate = lambda)
par(mfrow = c(1, 2))
hist(inv, breaks = 30, probability = TRUE, col = "lightblue",main = "Inverse-Transform Method", xlab = "x")
curve(dexp(x, rate = lambda), add = TRUE, col = "magenta", lwd = 2)
hist(exp, breaks = 30, probability = TRUE, col = "lightgreen",main = "Fungsi rexp", xlab = "x")
curve(dexp(x, rate = lambda), add = TRUE, col = "magenta", lwd = 2)
