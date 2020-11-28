#12.2.6
abar = 305
asd = 29
bbar = 311
bsd = 40
n1 = 66
n2 = 38
z = (abar - bbar) / sqrt(asd^2/n1 + bsd^2/n2)
a = 0.01
za = qnorm(1-a/2)
z
za
b = 0.02
L = (abar - bbar) + qnorm(1-b/2)*sqrt(asd^2/n1 + bsd^2/n2)
U = (abar - bbar) - qnorm(1-b/2)*sqrt(asd^2/n1 + bsd^2/n2)
print(c(L,U))

#12.3.1
di = c(-1,1,-3,2)
dbar = mean(di)
sd = sd(di)
t = dbar/(sd/sqrt(4))
t

#12.3.3
di = c(2,5,6,8,-6,4,18,-12,17,-7,16,12)
n = length(di)
sd = sd(di)
dbar = mean(di)
t = dbar / (sd / sqrt(n))
ta = qt(0.95, n-1)
print(c(t,ta))
a = 0.05
tb = qt(1-a/2,n-1)
L = dbar + tb*(sd/sqrt(n))
U = dbar - tb*(sd/sqrt(n))
print(c(L,U))

#12.3.5
di = c(2,1,-1,2,3,-1)
dbar = mean(di)
sd = sd(di)
t = dbar / (sd / sqrt(6))
ta = qt(0.95, 5)
print(c(t,ta))

#12.3.13 
di = c(2,3,15,-2,-1,1,-1,7,2,10)
dbar = mean(di)
sd = sd(di)
n = length(di)
t = dbar / (sd / sqrt(10))
pv = 1-pt(t, 9)
print(c(t,pv))
da = dbar + td*(sd/sqrt(10))
db = dbar - td*(sd/sqrt(10))
print(c(da,db))


#12.4.1
n1 = 100
n2 = 200
p1 = 0.5
p2 = 0.7
a = 0.05
pp = (p1*(1-p1))/n1 + (p2*(1-p2))/n2
L = (p1-p2) + qnorm(1-a/2)*sqrt(pp)
U = (p1-p2) - qnorm(1-a/2)*sqrt(pp)
print(c(L,U))
pi = (n1*p1 + n2*p2) / (n1+n2)
z = (p1-p2) / (sqrt(pi*(1-pi))*sqrt(1/n1 + 1/n2))
za = qnorm(1-a)
print(c(z,za)) #귀무가설기각

#12.4.3 
n1 = 120
n2 = 150
p1 = 52/120
p2 = 88/150
a = 0.05
ph = (n1*p1 + n2*p2) / (n1+n2)
z = (p1-p2) / (sqrt(ph*(1-ph))*sqrt(1/n1 + 1/n2))
pv = pnorm(z)
print(c(z,pv)) #귀무가설기각못함???
pp = (p1*(1-p1))/n1 + (p2*(1-p2))/n2
L = (p1-p2) + qnorm(1-a/2)*sqrt(pp)
U = (p1-p2) - qnorm(1-a/2)*sqrt(pp)
print(c(L,U))

#12.4.7
n1 = 85
n2 = 120
p1 = 21/85
p2 = 11/120
a = 0.01
ph = (n1*p1 + n2*p2) / (n1+n2)
z = (p1 - p2) / (sqrt(ph*(1-ph)) * sqrt(1/n1+1/n2))
za = qnorm(1-a)
print(c(z,za))  #기각


#12.4.15
n1 = 549
n2 = 534
p1 = 11/549
p2 = 70/534
a = 0.01
ph = (n1*p1+n2*p2) / (n1+n2)
z = (p1-p2) / (sqrt(ph*(1-ph))*sqrt(1/n1 + 1/n2))
za = qnorm(1-a)
print(c(z,za))  #귀무가설기각
b = 0.05
L = (p1 - p2) + qnorm(1-b/2)*sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
U = (p1 - p2) - qnorm(1-b/2)*sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
print(c(L,U))

