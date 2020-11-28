#-----------------------------------------
namelist = list()
o = matrix( c(378,237,26,388,196,25),
            nrow = 2, ncol = 3, byrow = True,
            dimanames = namelist)
row_sum = apply(o, 1, sum)
col_sum = apply(o, 2, sum)
r = length(row_sum)
c = length(col_sum)

e = (row_sum) %*%t(col_sum) / sum(o)
d = (o-e)^2 / e
chisq0 = sum(d)
alpha = 0.05
chisq_alpha = qchisq( 1-alpha, (r-1)*(c-1) )
print(c(chisq0, chisq_alpha))

df = (length(row_sum)-1)*(length(col_sum)-1)
pvalue = 1-pchisq(chisq0, (r-1)*(c-1))
pvalue



#2.9---------------------------------------
cumlative = c(0, pnorm(c(48, 60, 72), 60, 15), 1)
p = cumlative[2:5] - cumlative[1:4]
print(p)

o = c(16,28,36,20)
n = sum(o)
e = n*p
d = (e-o)^2/e
chisq0 = sum(d)
print(chisq0)
k = length(p)
pvalue = 1 - pchisq(chisq0, k-1)
print(c(chisq0, pvalue))


#-------------------------------------------
n = 3
p0 = 0.4
p = dbinom(c(0,1,2,3), n, p0)
print(p)

o = c(19,32,22,7)
n = sum(o)
e = 80*p
print(e)

d = (e-o)^2/e
chisq0 = sum(d)
print(chisq0)

pvalue = 1 - pchisq(chisq0, 2)
print(c(chisq0, pvalue))

#------------------------------------------------









