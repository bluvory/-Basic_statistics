#2
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
print(c(beta0_hat, beta1_hat))

plot(x,y)
x2 = seq(0, 10, by=0.1)
y2 = beta0_hat + beta1_hat*x2
lines(x2, y2, col = 'blue')

yhat = beta0_hat + beta1_hat*x
e = y - yhat
mse = sum(e^2)/(n-2)
print(mse)

