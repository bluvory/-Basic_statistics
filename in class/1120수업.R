a = c(10,20)
b = c(4,7,3)
a %*% t(b)

#2-------------------------
oA = c(37,24,19)
oB = c(17,33,20)
namelist = list( c("A","B"), c("양호", "보통", "불량"))
o = matrix( c(oA,oB), nrow=2, ncol=3, byrow=T,
            dimnames = namelist)
print(o)

d = (o-e)^2/e
chisq0 = sum(d)
print(c(chisq0, chisq_alpha))

#6--------------------------
oA = c(32,51,67,83,233)
oB = c(268,199,233,267.967)
namelist = list( c("사무지","교육자","기업인","상ㅇ"), c("알코올중독", "보통", "불량"))
o = matrix( c(oA,oB), nrow=2, ncol=4, byrow=T,
            dimnames = namelist)
print(o)

row_sum = apply(o,1,sum)
col_sum = apply(o,2,sum)
e = (row_sum %*% t(col_sum))/sum(o)
dimnames(e) = namelist

d = (o-e)^2/e
chisq0 = sum(d)
df = (length(row_sum)-1)*(length(col_sum)-1)
chisq_alpha = qchisq(1-alpha, df)
print(c(chisq0, chisq_alpha))
pvalue = 1-pchisq(chisq0, df)

#7-------------------------------------
oA = c(84,132)
oB = c(16,18)
namelist = list( c("싹틈","싹안틈"), c("처리", "처리안함"))
o = matrix( c(oA,oB), nrow=2, ncol=2, byrow=T,
            dimnames = namelist)
print(o)

row_sum = apply(o,1,sum)
col_sum = apply(o,2,sum)
e = (row_sum %*% t(col_sum))/sum(o)
print(e)
dimnames(e) = namelist

d = (o-e)^2/e
chisq0 = sum(d)
df = (length(row_sum)-1)*(length(col_sum)-1)
chisq_alpha = qchisq(1-alpha, df)
print(c(chisq0, chisq_alpha))
pvalue = 1-pchisq(chisq0, df)
pvalue

