#ex1 ------------------------------
o = c(18,55,27) ##관측도수
k = length(o) ##범주의 갯수
p = c(1/4, 1/2, 1/4)

k <- length(o)
n = sum(0)
e = p*n
d = (o-e)^2/e
chisq0 = sum(d)
alpha = 0.05
chisq_alpha = qchisq(1-alpha, k-1)
print(c(chisq0, chisq_alpha))
pvalue = 1-pchisq(chisq0, k-1)
print(pvalue)

m = matrix( c(o, e, d), nrow = 3, byrow = T,
            dimnames = list(c("O", "E", "d"), c("A", "B", "C")))
print(m)
s = apply(m, 1, sum)
cbind(m,s)

#ex2 -------------------------------




