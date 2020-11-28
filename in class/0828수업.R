#1---------------------
c <- 21.31
mu0 <- 20
n <- 70
sigma <- 5.6

a <-(c-mu0)/(sigma/sqrt(n))

prob <- 1-pnorm(a)

print(prob)

#2-----------------------
n <- 70
mu0 <- 20
sigma <- 5.6
alpha <- 0.95
z_alpha <- qnorm(1-alpha)
c <- mu0 + z_alpha*sigma/sqrt(n)

print(c)

#3------------------------
n <- 80
mu0 <- 1100
x <- 1060
sigma <- 210
a <- 0.01

z <- (x-mu0)/(sigma/sqrt(n))
z_alpha_half <- qnorm(1-a/2)

pvalue <- 2*(1-pnorm(abs(z)))
print(pvalue)
print(c(z, z_alpha_half))

