x <- seq(-5, 5, length.out = 110)
x

y <- (1/sqrt(2*pi))*exp(-x^2/2)
y

plot(x, y, type = 'l', col = 'red')

a <- 5
t <- (1/sqrt(pi*a))*(factorial((a+1)/2-1)/factorial(a/2-1))*(1+x^2/a)^(-(a+1)/2)

plot(x, t, type = 'l', col = 'blue')