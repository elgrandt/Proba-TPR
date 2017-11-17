###### INITIALIZATION ######
XpN1 <- numeric(3000)
XpN2 = numeric(1000)
XpN5 = numeric(1000)
XpN30 = numeric(1000)
XpN500 = numeric(1000)
N <- 6
P <- 1/9
EX1 <- N*P
VX1 <- N*P*(1-P)

###### EJERCICIO 1a ######
set.seed(1)
for (i in 1:3000){
  XpN1[i] <- mean(rbinom(n=i,size=N, prob=P)) 
}
plot(XpN1, main="Promedios con sem afuera", xlab = "N", ylab = "Promedio hasta N")

###### EJERCICIO 1b ######
for (i in 1:3000){
  set.seed(1)
  XpN1[i] <- mean(rbinom(n=i,size=N, prob=P)) 
}
plot(XpN1, main="Promedios con sem adentro", xlab = "N", ylab = "Promedio hasta N")

###### EJERCICIO 2a ######
n <- 2
for (i in 1:1000){
  XpN2[i] = mean(rbinom(n=n, size=N, prob=P))
}
qqnorm(XpN2, main="QQ-Plot")
hist(XpN2, main="Histrograma",prob=T)
boxplot(XpN2,main="Boxplot")

###### EJERCICIO 2b ######
n <- 5
for (i in 1:1000){
  XpN5[i] = mean(rbinom(n=n, size=N, prob=P))
}
qqnorm(XpN5, main="QQ-Plot")
hist(XpN5, main="Histrograma")
boxplot(XpN5,main="Boxplot")

###### EJERCICIO 2c1 ######
n <- 30
for (i in 1:1000){
  XpN30[i] = mean(rbinom(n=n, size=N, prob=P))
}
qqnorm(XpN30, main="QQ-Plot")
hist(XpN30, main="Histrograma",prob=T)
boxplot(XpN30,main="Boxplot")

###### EJERCICIO 2c2 ######
n <- 500
for (i in 1:1000){
  XpN500[i] = mean(rbinom(n=n, size=N, prob=P))
}
qqnorm(XpN500, main="QQ-Plot")
qqline(XpN500, col="red");
hist(XpN500, main="Histrograma",prob=T)
boxplot(XpN500,main="Boxplot")

###### EJERCICIO 2e ######
par(mfrow=c(1,4))
boxplot(XpN2,main="Boxplot N=2")
boxplot(XpN5,main="Boxplot N=5")
boxplot(XpN30,main="Boxplot N=30")
boxplot(XpN500,main="Boxplot N=500")

###### EJERCICIO 3b ######
XpN2estandarizada <- ((XpN2)-EX1)/sqrt(VX1/2)
XpN5estandarizada <- ((XpN5)-EX1)/sqrt(VX1/5)
XpN30estandarizada <- ((XpN30)-EX1)/sqrt(VX1/30)
XpN500estandarizada <- ((XpN500)-EX1)/sqrt(VX1/500)
par(mfrow=c(1,4))
boxplot(XpN2estandarizada,main="Boxplot estandarizado N=2")
boxplot(XpN5estandarizada,main="Boxplot estandarizado N=5")
boxplot(XpN30estandarizada,main="Boxplot estandarizado N=30")
boxplot(XpN500estandarizada,main="Boxplot estandarizado N=500")


qqnorm(XpN2estandarizada,main="QQ-Plot estandarizado N=2")
qqline(XpN2estandarizada,col="red")


qqnorm(XpN5estandarizada,main="QQ-Plot estandarizado N=5")
qqline(XpN5estandarizada,col="red")

qqnorm(XpN30estandarizada,main="QQ-Plot estandarizado N=30")
qqline(XpN30estandarizada,col="red")

qqnorm(XpN500estandarizada,main="QQ-Plot estandarizado N=500")
qqline(XpN500estandarizada,col="red")

###### EJERCICIO 3c ######
grilla <- seq(-4,4,by=0.1)
hist(XpN2estandarizada,main="Histrograma estandarizado N=2",prob=T)
lines(grilla,dnorm(grilla),col='red', lwd=2) 
hist(XpN5estandarizada,main="Histrograma estandarizado N=5",prob=T)
lines(grilla,dnorm(grilla),col='red', lwd=2) 
hist(XpN30estandarizada,main="Histrograma estandarizado N=30",prob=T)
lines(grilla,dnorm(grilla),col='red', lwd=2) 
hist(XpN500estandarizada,main="Histrograma estandarizado N=500",prob=T)
lines(grilla,dnorm(grilla),col='red', lwd=2) 
