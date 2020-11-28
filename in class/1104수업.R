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
sse = ((ni-1)*c(var(y1), var(y2), var(y3), var(y4)))
ss = c(sstr, sse, sst)
Df = c(k-1, n-k, n-1)
ms = ss/Df
F0 = ms[1]/ms[2]

anovaTb1 = data.frame("제곱합"=c(sstr, sse, sst), "자유도"=Df)
rownames(anovaTb1) = c("처리", "오차", "합계")
print(anovaTb1)

#ex2------------------------------------





