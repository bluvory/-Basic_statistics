#3----------------
n1 <- 50
mu1 <- 453
s1 <- 80
n2 <- 100
mu2 <- 401
s2 <- 60

mu <- mu1 - mu2
s <- s1^2/n1 + s2^2/n2
z <- qnorm(1-0.05/2)
d <- z*sqrt(s)
print(c(mu-d, mu+d))
pvalue <- (1-pnorm(z)*2)
print(pvalue)

#4-----------------
z <- mu / sqrt(s)
print(z)

#5-------------------
data1 <- c(8, 5, 7, 6, 9, 7)
data2 <- c(2, 6, 4, 7, 6)
x1 <- sd(data1)
x2 <- sd(data2)
n1 <- length(data1)
n2 <- length(data2)
s <- ((n1-1)*(x1^2) + (n2-1)*(x2^2))/(n1+n2-2)
print(s)

#6------------------
data1 <- c(44,44,56,46,47,38,58,53,49,35,46,30,41)
data2 <- c(35,47,55,29,40,39,32,41,42,57,51,39)
n1 <- length(data1)
n2 <- length(data2)
x1 <- mean(data1)
x2 <- mean(data2)
s1 <- sd(data1)
s2 <- sd(data2)
sp <- sqrt((n1*s1^2 + n2*s2^2)/(n1+n2-2))
print(sp)
print(c(n1, n2, x1, x2, s1, s2))
d <- qt(0.975, 23)*sp*(sqrt((1/n1)+(1/n2)))
print(c(x1-x2-d, x1-x2+d))

#7----------------------
t = (x1-x2)/sp*sqrt(1/n1 + 1/n2)
print(t)

#8----------------------
n1 <-13
x <- 2.4
s1 <- 0.72
n2 <- 11
y <- 2.15
s2 <- 0.35
t <- (x-y)/sqrt(s1^2/n1 + s2^2/n2)
ta <- qt(0.975, 10)
if (abs(t)>=ta){
  print("기각")
} else {
  print("안기각")
}



