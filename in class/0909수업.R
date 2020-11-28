#1-----------------
qt(0.9, 2)
qt(0.9, 10)

#2------------------
qt(0.95, 9)

#3-----------------
n <- 15
xbar <- 39.3
s <- 2.6
a <- 0.1
t_a <- qt(1-a/2, n-1)
d <- t_a*s/sqrt(n)
print(c(xbar-d, xbar+d))

#4--------------------
data <- c(175, 190, 215, 198, 184, 207, 210, 193, 196, 180)
xbar <- mean(data)
s <- sd(data)
n <- 10
mu0 <- 200
a <- 0.05

t0 <- (xbar-mu0)/(s/sqrt(n))
t_alpha <- qt(1-a/2, n-1)
pvalue <- pt(t0, n-1)


if(t0 <= - t_alpha) {
  print("reject H0")
} else {
  print("do not reject H0")
}

print(c(t0, t_alpha, pvalue))

#5--------------------
n <- 9
xbar <- 8.3
mu0 <- 8.5
s <- 1.2
a <- 0.05
t_a <- qt(1-a/2, n-1)
d <- t_a*s/sqrt(n)
print(c(xbar-d, xbar+d))

t0 <- (xbar-mu0)/(s/sqrt(n))
t_alpha <- qt(1-a/2, n-1)
pvalue <- pt(t0, n-1)


if(t0 <= - t_alpha) {
  print("reject H0")
} else {
  print("do not reject H0")
}

print(c(t0, t_alpha, pvalue))

