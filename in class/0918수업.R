data <- seq(-4, 4, length.out = 101)

plot(data, dnorm(data, mean=0, sd=1), col="blue", xlim=c(-4,4), ylim=c(0,0.4))
par(new=T)
plot(data, dt(data,5), type = "l", col="red")
par(new=T)
plot(data, dt(data,10), type = "l", col="black")
par(new=T)
plot(data, dt(data,30), type = "l", col="green")
