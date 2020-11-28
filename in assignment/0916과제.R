data <- rnorm(n*N, mu, sigma)
data <- matrix(data, nrow=N, ncol=n)


#1-----------------------
n <- 30
mu <- 3
sigma <- 4
N <- 10000
xbar <- apply(data, 1, mean)
x <- seq(-2, 8, by = 0.2)
hist(xbar, probability = T)
lines(x, dnorm(x, mu, sigma/sqrt(n)), col = "blue")

#2----------------------
n <- 30
mu <- 3
sigma <- 4
N <- 10000
xbar <- apply(data, 1, mean)
x <- seq(-2, 8, by = 0.2)
z <- (xbar - mu)/(sigma/sqrt(n))
hist(z, probability = T)
lines(x, dnorm(x, 0, 1), col = "blue")

#3---------------------
n <- 30
mu <- 3
sigma <- 4
N <- 10000
xbar <- apply(data, 1, mean)
x <- seq(-2, 8, by = 0.2)
s <- apply(data, 1, sd)
lines(x, dt(x, n-1), col="red")

#4----------------------
n <- 7
mu <- 3
sigma <- 4
N <- 10000
xbar <- apply(data, 1, mean)
x <- seq(-4,10, by = 0.2)
hist(xbar,breaks = x, probability = T)
lines(x, dnorm(x, mu, sigma/sqrt(n)), col = "blue")

#5-----------------------
n <- 7
mu <- 3
sigma <- 4
N <- 10000
xbar <- apply(data, 1, mean)
x <- seq(-2, 8, by = 0.2)
z <- (xbar - mu)/(sigma/sqrt(n))
hist(z, probability = T)
lines(x, dnorm(x, 0, 1), col = "blue")

#6-----------------------
n <- 7
mu <- 3
sigma <- 4
N <- 10000
xbar <- apply(data, 1, mean)
x <- seq(-2, 8, by = 0.2)
s <- apply(data, 1, sd)
lines(x, dt(x, n-1), col="red")

