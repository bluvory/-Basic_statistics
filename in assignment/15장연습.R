#ex1 ------------------------------
o = c(18,55,27) ##관측도수
k = length(o) ##범주의 갯수
p = c(1/4, 1/2, 1/4)

n = sum(o)
e = p*n
d = (o-e)^2/e
x2 = sum(d)
alpha = 0.05
x2a = qchisq(1-alpha, k-1)
print(c(x2, x2a))
pvalue = 1-pchisq(x2, k-1)
print(pvalue)

m = matrix( c(o, e, d), nrow = 3, byrow = T,
            dimnames = list(c("O", "E", "d"), c("A", "B", "C")))
print(m)
s = apply(m, 1, sum)
cbind(m,s)


#ex2??-------------------------
oA = c(37,24,19)
oB = c(17,33,20)
namelist = list( c("A","B"), c("양호", "보통", "불량"))
o = matrix( c(oA,oB), nrow=2, ncol=3, byrow=T,
            dimnames = namelist)
print(o)

d = (o-e)^2/e
chisq0 = sum(d)
print(c(chisq0, chisq_alpha))


#2.1------------------------------
o = c(38,61,54,65,55,37)
n = 310
p = c(1/6,1/6,1/6,1/6,1/6,1/6)
e = n*p
d = (o-e)^2/e
x2 = sum(d)
a = 0.05
x2_a = qchisq(1-a,5)
print(c(x2,x2_a))

m = matrix(c(o,e,d),nrow=3,byrow = T,
           dimnames = list(c('o','e','d'),c('1','2','3','4','5','6')))
s = apply(m,1,sum)
cbind(m,s)


#2.7---------------------------------
o = c(52.09, 54.46, 52.68, 51.68, 53.83, 47.21, 44.36)
n = 356.31
p = c(1/7,1/7,1/7,1/7,1/7,1/7,1/7)
e = n*p
d = (o-e)^2/e
x2 = sum(d)
a = 0.01
x2_a = qchisq(1-a, 6)
print(c(x2, x2_a))


#2.9-----------------------------------
cumlative = c(0, pnorm(c(48, 60, 72), 60, 15), 1)
p = cumlative[2:5] - cumlative[1:4]
print(p)

o = c(16,28,36,20)
n = sum(o)
e = n*p
d = (e-o)^2/e
x2 = sum(d)
k = length(p)
pvalue = 1 - pchisq(x2, k-1)
print(c(x2, pvalue))


#2.11----------------------------------
n = 3
p0 = 0.4
p = dbinom(c(0,1,2,3), n, p0)
print(p)

o = c(19,32,22,7)
e = 80*p
print(e)

d = (e-o)^2/e
a = 0.05
x2 = sum(d)
x2a = qchisq(1-a, 2)

pvalue = 1 - pchisq(x2, 2)
print(c(x2, pvalue))


#ex6--------------------------
oA = c(32,268)
oB = c(51,199)
oC = c(67, 233)
oD = c(83,267)
namelist = list( c("사무지","교육자","기업인","상인"), c("알코올중독", "정상"))
o = matrix( c(oA,oB,oC,oD), nrow=4, byrow=T,
            dimnames = namelist)
print(o)

rs = apply(o,1,sum)
cs = apply(o,2,sum)
e = (rs %*% t(cs))/sum(o)
dimnames(e) = namelist
print(e)

d = (o-e)^2/e
x2 = sum(d)
df = (length(rs)-1)*(length(cs)-1)
a = 0.05
x2a = qchisq(1-a, df)
print(c(x2, x2a))
pvalue = 1-pchisq(x2, df)
print(pvalue)


#ex7-------------------------------------
oA = c(84,16)
oB = c(132,18)
namelist = list( c("처리", "처리안함"), c("싹틈","싹안틈"))
o = matrix( c(oA,oB), nrow=2, ncol=2, byrow=T,
            dimnames = namelist)
print(o)

row_sum = apply(o,1,sum)
col_sum = apply(o,2,sum)
e = (row_sum %*% t(col_sum))/sum(o)
dimnames(e) = namelist
print(e)

d = (o-e)^2/e
x2 = sum(d)
df = (length(row_sum)-1)*(length(col_sum)-1)
a = 0.05
x2a = qchisq(1-a/2, df)
print(c(x2, x2a))
pvalue = 1-pchisq(x2, df)
pvalue


#3.1------------------------------------
oA = c(32, 8)
oB = c(28, 12)
oC = c(19, 21)
namelist = list( c("양상추", "시금치", "토마토"), c("심각", "심각안함"))
o = matrix( c(oA,oB,oC), nrow=3, byrow=T,
            dimnames = namelist)
print(o)

row_sum = apply(o,1,sum)
col_sum = apply(o,2,sum)
e = (row_sum %*% t(col_sum))/sum(o)
dimnames(e) = namelist
print(e)

d = (o-e)^2/e
x2 = sum(d)
a = 0.1
x2_a = qchisq(1-a, 2)
print(c(x2, x2_a))

a = 0.05
z = qnorm(1-a/2)
n = 40
p1 = 32/40
p2 = 28/40
p3 = 19/40
Al = p1 + z*sqrt((p1*(1-p1))/n)
Au = p1 - z*sqrt((p1*(1-p1))/n)
Bl = p2 + z*sqrt((p2*(1-p2))/n)
Bu = p2 - z*sqrt((p2*(1-p2))/n)
Cl = p3 + z*sqrt((p3*(1-p3))/n)
Cu = p3 - z*sqrt((p3*(1-p3))/n)
print(c(Al, Au, Bl, Bu, Cl, Cu))


#3.5------------------------------------
oA = c(58,57)
oB = c(43,77)
oC = c(56,42)
oD = c(45,75)
namelist = list(c("A", "B", "C", "D"), c("죽음", "삶"))
o = matrix(c(oA, oB, oC, oD), nrow = 4, byrow = T,
           dimnames = namelist)

row_sum = apply(o,1,sum)
col_sum = apply(o,2,sum)
e = (row_sum %*% t(col_sum))/sum(o)
dimnames(e) = namelist
print(e)

d = (o-e)^2/e
x2 = sum(d)
a = 0.1
x2_a = qchisq(1-a, 3)
print(c(x2, x2_a))


#3.6-------------------------------------------------
oA = c(58,57)
oC = c(56,42)
namelist = list(c("A", "C"), c("죽음", "삶"))
o = matrix(c(oA, oC), nrow = 2, byrow = T,
           dimnames = namelist)

row_sum = apply(o,1,sum)
col_sum = apply(o,2,sum)
e = (row_sum %*% t(col_sum))/sum(o)
dimnames(e) = namelist
print(e)

d = (o-e)^2/e
x2 = sum(d)
a = 0.05
x2a = qchisq(1-a, 1)
print(c(x2, x2a))

n1 = 115
n2 = 98
pa = 58/n1
pc = 56/n2
ph = (58+56) / (n1+n2)
z = (pa - pc) / (sqrt(ph*(1-ph)) * sqrt(1/n1+1/n2))
za = qnorm(1-a/2)
print(c(z,za))


#3.7---------------------------------------------
oA = c(38,15,7)
oB = c(22,32,16)
oC = c(15,30,25)
namelist = list(c("아무조치안함", "물리치료", "운동요법"), c("상당손실", "작은변화", "상당증가"))
o = matrix(c(oA,oB,oC), nrow = 3, byrow=T,
           dimnames = namelist)
rs = apply(o,1,sum)
cs = apply(o,2,sum)
e = (rs %*% t(cs)) / sum(o)
dimnames(e) = namelist
print(e)

d = (o-e)^2/e
x2 = sum(d)
a = 0.05
x2_a = qchisq(1-a, 4)
print(c(x2, x2_a))


#ex8-----------------------------------------
namelist = list(c("남자", "여자"), c("너무많다","적당하다","너무적다"))
o = matrix( c(378,237,26,388,196,25),
            nrow = 2, ncol = 3, byrow = T,
            dimnames = namelist)
rs = apply(o, 1, sum)
cs = apply(o, 2, sum)
r = length(rs)
c = length(cs)

e = ((rs) %*%t(cs)) / sum(o)
d = (o-e)^2 / e
x2 = sum(d)
a = 0.05
x2a = qchisq( 1-a, (r-1)*(c-1) )
print(c(x2, x2a))

df = (length(rs)-1)*(length(cs)-1)
pvalue = 1-pchisq(x2, (r-1)*(c-1))
pvalue


#4.1--------------------------------------
oA = c(59,108,17)
oB = c(70,63,3)
namelist = list(c("자신", "변호사"), c("증가", "같음", "감소"))
o = matrix(c(oA, oB), nrow = 2, byrow = T,
           dimnames = namelist)
rs = apply(o,1,sum)
cs = apply(o,2,sum)
e = (rs %*% t(cs)) / sum(o)
dimnames(e) = namelist
print(e)


d = (o-e)^2/e
x2 = sum(d)
a = 0.05
x2a = qchisq(1-a, 2)
print(c(x2, x2a))


#4.5--------------------------------------
oA = c(65,118-65)
oB = c(59,135-59)
oC = c(48,90-48)
oD = c(43,92-43)
namelist = list(c("서부", "동부", "남부", "중부"), c("올리브", "시러"))
o = matrix(c(oA, oB, oC, oD), nrow = 4, byrow=T,
           dimnames = namelist)

rs = apply(o,1,sum)
cs = apply(o,2,sum)
e = (rs%*%t(cs)) / sum(o)
dimnames(e) = namelist
print(e)

d = (o-e)^2/e
a = 0.05
x2 = sum(d)
x2_a = qchisq(1-a, 3)
print(c(x2, x2_a))




