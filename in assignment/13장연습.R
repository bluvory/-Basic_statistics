#13.3.1
x = c(1,2,3,4,5)
y = c(0.9,2.1,2.5,3.3,3.8)
xbar = mean(x)
ybar = mean(y)
s_xx= sum((x-xbar)^2)
s_yy = sum((y-ybar)^2)
s_xy = sum((x-xbar)*(y-ybar))
b1 = s_xy / s_xx
b0 = ybar - b1*xbar
print(c(b0,b1))

#13.3.7
x = c(1,2,3,4,5)
y = c(0.9,2.1,2.5,3.3,3.8)
n = length(y)
xbar = mean(x)
ybar = mean(y)
s_xx= sum((x-xbar)^2)
s_yy = sum((y-ybar)^2)
s_xy = sum((x-xbar)*(y-ybar))
b1 = s_xy / s_xx
b0 = ybar - b1*xbar
yhat = b0 + b1*x
e = y - yhat
sum(e)
sse = sum(e^2)
sse2 = s_yy - s_xy^2/s_xx
mse = sse / (n-2)
mse

#13.3.9
n = 18
xbar = 1.2
ybar = 5.1
sxx = 14.10
sxy = 2.31
syy = 2.01
b1 = sxy/sxx
b0 = ybar - xbar*b1
#직선식은 y=b0 + b1*x +s
sse = syy - sxy^2/sxx
mse = sse / (n-2)
print(c(sse,mse))


#13.3.13
x = c(6,7,5,21,13,5,13,14)
y = c(28,23,29,22,20,19,28,19)
n = 8
xbar = mean(x)
ybar = mean(y)
sxx = sum((x-xbar)^2)
syy = sum((y-ybar)^2)
sxy = sum((x-xbar)*(y-ybar))
b1hat = sxy / sxx
b0hat = ybar - b1hat*xbar
print(c(b1hat, b0hat))
y = b0hat + 8*b1hat
print(y)

#13.4.1
x = c(0,1,6,3,5)
y = c(4,3,0,2,1)
n = 5
xbar = mean(x)
ybar = mean(y)
sxx = sum((x-xbar)^2)
syy = sum((y-ybar)^2)
sxy = sum((x-xbar)*(y-ybar))
b1 = sxy / sxx
b0 = ybar - xbar*b1
sse = syy - sxy^2/sxx
s = sse/(n-2)
t = b1 / (s/sqrt(sxx))
a = 0.05
ta = qt(1-a/2, 3)
print(c(t,ta))
y = b0 + b1*2.5
b = 0.1
tb = qt(1-b/2,3)
L = b0 + tb*s*sqrt((1/n)+xbar^2/sxx)
U = b0 - tb*s*sqrt((1/n)+xbar^2/sxx)
print(c(L,U))  #4


#13.4.3
x = c(1,2,3,4,5)
y = c(0.9,2.1,2.4,3.3,3.8)
n = 5
xbar = mean(x)
ybar = mean(y)
sxx = sum((x-xbar)^2)
syy = sum((y-ybar)^2)
sxy = sum((x-xbar)*(y-ybar))
b1hat = sxy / sxx
b0hat = ybar - xbar*b1hat
sse = syy - sxy^2/sxx
s = sse / (n-2)  #1

a = 0.05
t = (b1hat - 1) / (sqrt(s)/sqrt(sxx))
ta = qt(1-a/2, n-2)
print(c(t,ta))  #2

y = b0hat + b1hat*3.5
L = (b0hat + b1hat*3.5) + ta*sqrt(s)*sqrt(1+1/n + (3.5-xbar)^2/sxx)
U = (b0hat + b1hat*3.5) - ta*sqrt(s)*sqrt(1+1/n + (3.5-xbar)^2/sxx)
print(c(L,U))  #3

b = 0.1
tb = qt(1-b/2, n-2)
L1 = b0hat + tb*sqrt(s)*sqrt(1/n + xbar^2/sxx)
U1 = b0hat - tb*sqrt(s)*sqrt(1/n + xbar^2/sxx)
print(c(L1, U1))  #4

#13.4.7
n = 15
xbar = 10.8
ybar = 122.7
sxx = 70.6
syy = 98.5
sxy = 68.3
a = 0.05
ta = qt(1-a/2, n-2)
b1hat = sxy/sxx
b0hat = ybar - b1hat*xbar
sse = syy - sxy^2/sxx
s = sqrt(sse/(n-2))

x1 = 12
y1 = b0hat + b1hat*x1
L = y1 + ta*s*sqrt(1/n + (x1-xbar)^2/sxx)
U = y1 - ta*s*sqrt(1/n + (x1-xbar)^2/sxx)
print(c(L, U))

x2 = 15
y2 = b0hat + b1hat*x2
K = y2 + ta*s*sqrt(1/n + (x2-xbar)^2/sxx)
J = y2 - ta*s*sqrt(1/n + (x2-xbar)^2/sxx)
print(c(K, J))

(L-U)/2
(K-J)/2

#13.4.9
x = c(0,1,2,3,4)
y = c(195,216,244,260,284)
n = 5
xbar = mean(x)
ybar = mean(y)
sxx = sum((x-xbar)^2)
syy = sum((y-ybar)^2)
sxy = sum((x-xbar)*(y-ybar))

b1hat = sxy / sxx
b0hat = ybar - b1hat*xbar
plot(x,y)
abline(a=b0hat, b=b1hat)  #1

a = 0.05
ta = qt(1-a/2, n-2)
sse = syy - sxy^2/sxx
s = sqrt(sse/(n-2))
t = b1hat / (s / sqrt(sxx))
print(c(t,ta))  #2

x1 = 9
y1 = b0hat + b1hat*x1
#자료의범위를벗어났으므로예측하는데무리가있다


#13.5.1
n = 14
xbar = 1.2
ybar = 5.1
sxx = 14.10
sxy = 2.31
syy = 2.01
sst = syy
ssr = sxy^2 / sxx
ssr/sst

#13.5.3
sxx = 92
syy = 457
sxy = 160
sst = syy
ssr = sxy^2 / sxx
ssr/sst

#13.5.5
x = c(200.8, 194.6, 183.5, 190.5, 210.2, 170.5, 220.0)
y = c(207.0, 199.0, 188.0, 191.2, 211.0, 176.2, 218.0)
xbar = mean(x)
ybar = mean(y)
sxx = sum((x-xbar)^2)
syy = sum((y-ybar)^2)
sxy = sum((x-xbar)*(y-ybar))
sst = syy
ssr = sxy^2/sxx
ssr/sst
sqrt(ssr/sst)

#13.6.1
yhat = c(11.3,14.8,18.4,22.0,25.5,27.0,31.2,32.7,34.1,36.2, 39.8, 43.4, 45.5, 46.2, 46.9, 46.9)
e = c(-0.3,0.2,-5.4,2.0,0.5,5.0,-9.2,6.3,12.9,-4.2, -14.8, 7.6, -1.5, 15.8, 1.1, -16.9)
plot(yhat, e)


#13.6.3
yhat = c(2.2,3.1,2.5,3.3,2.3,3.6,2.6,2.5,3.0,3.2,2.9,3.3,2.7,3.2)
e = c(-1,2,3,-3,-1,5,0,0,3,-2,2,-5,0,1)
plot(yhat, e)

h = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)
ei = c(-3,-5,0,-2,1,-2,-1,0,-1,0,2,3,3,5)
plot(h,ei)




