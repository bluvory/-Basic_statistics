#1------------------
n <- 30
mu <- 3
sigma <- 4
N <- 10000


data <- rnorm(n*N, mu, sigma)
data <- matrix(data, nrow=N, ncol=n)
xbar <- apply(data, 1, mean)
t <- (xbar - mu)/(sigma/sqrt(n))
s <- apply(data, 1, sd)

x <- seq(-2, 8, by = 0.2)
hist(xbar, breaks = x, probability = T)
lines(x, dnorm(x, mu, sigma/sqrt(n)), col = "blue")
lines(x, dnorm(x, 0, 1), col = "blue")
lines(x, dt(x, n-1), col="red")
