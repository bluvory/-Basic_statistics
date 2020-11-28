#ป๙ลื02
di = c(1,3,11,2,3,4,1,3,11)
dbar = mean(di)
n = length(di)
sd = sd(di)
t = dbar/ (sd/sqrt(n))
print(1-pt(t,n-1))
L = dbar + ta*sd/sqrt(n)
U = dbar - ta*sd/sqrt(n)
print(c(L,U))
#

#ป๙ลื03
n1 = 60
n2 = 170
p1 = 51/60
p2 = 121/170
phat = (n1*p1 + n2*p2) / (n1+n2)
z = (p1 - p2) / ((sqrt(phat*(1-phat)))*sqrt(1/n1 + 1/n2))
print(1-pnorm(z)) #pvalue

#ป๙ลื04
x = c(1,2,2,3,3,4,4,5,7,8,11,11)
y = c(5.32,4.92,5.1,4.2,3.9,3.4,3.1,2.8,2.54,2.1,1.3,1.2)
n = length(x)
xbar = mean(x)
ybar = mean(y)
sxx = sum((x-xbar)^2)
syy = sum((y-ybar)^2)
sxy = sum((x-xbar)*(y-ybar))
b1hat = sxy / sxx
b0hat = ybar - b1hat*xbar
plot(x,y)
abline(a=b0hat, b=b1hat)  #2

a = 0.05
sse = syy - sxy^2/sxx
s = sqrt(sse/(n-2))
b1L = b1hat + qt(1-a/2, n-2)*(s/sqrt(sxx))
b1U = b1hat - qt(1-a/2, n-2)*(s/sqrt(sxx))
print(c(b1L, b1U)) #3

t = b1hat / (s/sqrt(sxx))
ta = qt(1-a, n-2)
print(c(t,ta)) #4

xstar = 9
sse = syy - sxy^2/sxx
s = sqrt(sse/(n-2))
tal = qt(1-a/2, n-2)
yL = (b0hat + b1hat*xstar) + tal*s*sqrt(1+1/n+(xstar-xbar)^2/sxx)
yU = (b0hat + b1hat*xstar) - tal*s*sqrt(1+1/n+(xstar-xbar)^2/sxx)
print(c(yL, yU)) #5

R = sxy^2 / (sxx * syy)
print(R) #6

yhat = b0hat + b1hat*x
ei = y-yhat
plot(ei, yhat)  #7





