#9
x <- c(7.,80,72,76,76,76,72,78,82,64,74,92,74,68,84)
y <- c(68,72,62,70,58,66,68,52,64,72,74,60,74,72,74)
d = x-y
m = mean(d)
s = sd(d)
a = 0.05
n= 15
t = qt(1-a/2, n-1)
print(c(m-t*s/sqrt(n), m+t*s/sqrt(n)))

b = 0.01
t0 = m / s/sqrt(n)
ta = qt(1-b, n-1)
if (t > t0) {
  print("기각")
} else {
  print("기각안함")
}


#10
p1 = 88/100
p2 = 126/150
p0 = p1-p2
pa = p1*(1-p1)/100
pb = p2*(1-p2)/150
d = sqrt(pa + pb)
a = 0.05
z = qnorm(1-a/2)
print(c(p0-z*d, p0+z*d))

#11
p1 = 88/100
p2 = 126/150
p0 = p1-p2
z = p0/sqrt(1/100+1/150)
















