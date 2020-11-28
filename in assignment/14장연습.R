#ex1 ------------------------------
y1 = c(10,15,8,12,15)
y2 = c(14,18,21,15)
y3 = c(17,16,14,15,17,15,18)
y4 = c(12,15,17,15,16,15)

k = 4
ni = c(length(y1), length(y2), length(y3), length(y4))
yibar = c(mean(y1), mean(y2), mean(y3), mean(y4))
n = sum(ni)
y = c(y1, y2, y3, y4)
ybar = mean(y)

sst = sum((y - ybar)^2)
sst2 = var(y)*(n-1)
sstr = sum(ni*(yibar - ybar)^2)
sse = sst - sstr
sse2 = sum((ni-1)*c(var(y1),var(y2),var(y3),var(y4)))
ss = c(sstr, sse, sst)
Df = c(k-1, n-k, n-1)
ms = ss/Df
F0 = ms[1]/ms[2]

anovaTb1 = data.frame("제곱합"=c(sstr, sse, sst),"자유도"=Df,'평균제곱'=ms)
rownames(anovaTb1) = c("처리", "오차", "합계")
print(anovaTb1)


#ex1.1---------------------------------
y1 <- c(10, 15,  8, 12, 15)
y2 <- c(14, 18, 21, 15)
y3 <- c(17, 16, 14, 15, 17, 15, 18)
y4 <- c(12, 15, 17, 15, 16, 15)

ni <- c(length(y1), length(y2), length(y3), length(y4))
group <- rep(c('A', 'B', 'C', 'D'), ni)
data <- data.frame( '마모도' = c(y1, y2, y3, y4), '코팅' = group )
print(data)

fit <- lm(마모도 ~ 코팅, data)
aTbl <- anova(fit)
print(aTbl)


#2.1--------------------------------
y1 = c(6,10)
y2 = c(9,5)
y3 = c(9,7)
y4 = c(4,6)

k = 4
ni = 2
yibar = c(mean(y1), mean(y2), mean(y3), mean(y4))
n = 8
y = c(y1, y2, y3, y4)
ybar = mean(y)

sst = sum((y-ybar)^2)
sstr = sum(ni*(ybar-yibar)^2)
sse = sst - sstr
ss = c(sstr, sse, sst)
Df = c(k-1, n-k, n-1)
ms = ss/Df
F0 = ms[1]/ms[2]

anovaTb1 = data.frame("제곱합"=c(sstr, sse, sst),"자유도"=Df,'평균제곱'=ms)
rownames(anovaTb1) = c("처리", "오차", "합계")
print(anovaTb1)


#2.5--------------------------------------
y1 = c(2,1,3)
y2 = c(1,5)
y3 = c(9,5,6,4)
y4 = c(3,4,5)
y = c(y1, y2, y3, y4)
yibar = c(mean(y1), mean(y2),mean(y3),mean(y4))
ybar = mean(y)
ni = c(length(y1),length(y2),length(y3),length(y4))
k = 4
n = sum(ni)

sst = sum((y-ybar)^2)
sstr = sum(ni*(yibar-ybar)^2)
sse = sst - sstr
ss = c(sstr, sse, sst)
Df = c(k-1, n-k, n-1)
ms = ss/Df
F0 = ms[1]/ms[2]
1-pf(F0, k-1, n-k)

anovaTb1 = data.frame("제곱합"=c(sstr,sse,sst), "자유도"=Df, "평균제곱"=c(ms[1],ms[2],''), "F값"=c(F0,'',''))
rownames(anovaTb1) = c("처리", "오차", "합계")
print(anovaTb1)


#2.6--------------------------------------
yibar = c(5,2,7)
ni = c(10,6,9)
n = sum(ni)
ybar = sum(ni*yibar)/n
k = 3
sse = 30+16+25
sstr = sum(ni*(yibar-ybar)^2)
sst = sstr+sse
ss = c(sstr, sse, sst)
Df = c(k-1, n-k, n-1)
ms = ss/Df

anovaTb1 = data.frame("제곱합" = c(sstr, sse, sst), "자유도"=Df, "평균제곱"=c(ms[1],ms[2],''))
rownames(anovaTb1) = c("처리", "오차", "합계")
print(anovaTb1)


#3.8------------------------------------
y1 = c(0.95,0.86,0.71,0.72,0.74)
y2 = c(0.71,0.85,0.62,0.72,0.64)
y3 = c(0.69,0.68,0.51,0.73,0.44)
y = c(y1, y2, y3)
yibar = c(mean(y1), mean(y2), mean(y3))
ybar = mean(y)
k = 3
ni = c(5,5,5)
n = sum(ni)

sst = sum((y-ybar)^2)
sstr = sum(ni*(yibar-ybar)^2)
sse = sst - sstr
ss = c(sstr, sse, sst)
Df = c(k-1, n-k, n-1)
ms = ss/Df
f = ms[1]/ms[2]
anovaTb1 = data.frame("제곱합"=c(sstr, sse, sst), "자유도"=Df, "평균제곱합"=ms, "F"=f)
rownames(anovaTb1) = c("처리", "오차", "합계")
print(anovaTb1)




