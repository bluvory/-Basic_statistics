#12----------------------
xbar <- 14.6
s <- 3.0
n <- 70
alpha <- 0.025
mu0 <- 15
z <- (xbar-mu0)/(s/sqrt(n))
z_alpha_half <- qnorm(1-alpha)
pvalue = (1-pnorm(abs(z)))

print(c(z,z_alpha_half,pvalue))

#13(1)--------------------
n <- 750
p <- 0.598
pnorm(abs(sqrt(p*(1-p)))/sqrt(n))
d = qnorm(1 - 0.025)*sqrt(p*(1-p)/n)
print(c(p - d, p + d))

#13(2)---------------------
n <- 750
p <- 0.042
d = qnorm(1 - 0.05)*sqrt(p*(1-p)/n)
print(c(p - d, p + d))

#14-------------------------
n <- 500
p <- 228/500
a <- 0.05
z <- (p - 0.5)/sqrt(0.5*0.5/n)
z_alpha_half <- qnorm(1-a)
pvalue = (1-pnorm(abs(z)))
print(c(pvalue, z_alpha_half))
