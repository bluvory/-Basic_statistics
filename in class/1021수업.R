#6
x = c(3,3,4,5,6,6,7,8,8,9)
y = c(9,5,12,9,14,16,22,18,24,22)
xbar = mean(x)
ybar = mean(y)
n = length(x)

s_xx= sum(x^2) - n*xbar^2
s_yy = sum((y-ybar)^2)
s_xy = sum((x-xbar)*(y-ybar))
s_xy2 = sum(x*y) - n*xbar*ybar

beta1_hat = s_xy / s_xx
beta0_hat = ybar - beta1_hat*xbar
SSE <- s_yy - s_xy^2/s_xx
s <- sqrt(SSE/(n-2))

xast <- 9.5
se <- s*sqrt( 1/n + (xast-xbar)^2/s_xx)

alpha <- 0.05
t_alpha_2 <- qt(1-alpha/2, n-2)

yhat <- beta0_hat + beta1_hat*xast

interval <- c(yhat - t_alpha_2*se, yhat + t_alpha_2*se)
print(interval)


#7







