#6---------------
qchisq(1-0.05, 17)
qchisq(1-0.95, 17)

#7--------------
n <- 10
s <- 0.4
alpha <- 0.1

chisq_alpha_2 <- qchisq(1-alpha/2, n-1)
chisq_1_alpha_2 <- qchisq(alpha/2, n-1)

a <- s*sqrt( (n-1) / chisq_alpha_2 )
b <- s*sqrt( (n-1) / chisq_1_alpha_2 )
print(c(a,b))

#8----------------
n <- 10
s <- 0.4
m <- 0.2
x <- (n-1)*(s^2)/(0.2^2)
a <- qchisq(1-0.95, n-1)

if(x >= a) {
  print("reject H0")
} else {
  print("do not reject H0")
}
pvalue <- 1 - pchisq(x, n-1)
print(pvalue)

#9----------------


