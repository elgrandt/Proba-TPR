par(mfrow=c(4,5))
Xn <- numeric(3000)
lambda <- 1/4
set.seed(1)
for (i in 1:3000){
  muestra <- rexp(n=i,rate=lambda)
  Xn[i] <- mean(muestra) 
}
plot(Xn, main="Promedios con sem afuera", xlab = "N", ylab = "Promedio hasta N")

for (i in 1:3000){
	set.seed(1)  
muestra <- rexp(n=i,rate=lambda)
  	Xn[i] <- mean(muestra) 
}
plot(Xn, main="Promedios con sem adentro", xlab = "N", ylab = "Promedio hasta N")


Xp <- numeric(1000)
x1 <-numeric(1000)
x2 <-numeric(1000)
for(i in 1:1000) {
	x1[i] <- rexp(n=1, rate=lambda)
	x2[i] <- rexp(n=1, rate=lambda)
	Xp[i] <- ((x1[i]+x2[i])/2)	
}
qqnorm(Xp)
hist(Xp)
boxplot(Xp,xlab="",ylab="valores xp",main="boxplot xp")

Xp <- numeric(1000)
n <- 5
mat <- matrix(nrow=n,ncol = 1000)
for(i in 1:1000) {
	for(j in 1:n) {
		mat[j,i] <- rexp(n=1, rate=lambda)	
	}
}
Xp <- numeric(1000)
for(k in 1:1000){
	s <- sum(mat[,k])
	
	Xp[k] <- s/n
}
boxplot(Xp,main="con 5 va")
hist(Xp, main="histograma con 5")
qqnorm(Xp, main="qqnorm xp con 5")
qqnorm(mat, main="qqnorm mat con 5")



Xp <- numeric(1000)
n <- 30
mat <- matrix(nrow=n,ncol = 1000)
for(i in 1:1000) {
	for(j in 1:n) {
		mat[j,i] <- rexp(n=1, rate=lambda)	
	}
}
Xp <- numeric(1000)
for(k in 1:1000){
	s <- sum(mat[,k])
	
	Xp[k] <- s/n
}
boxplot(Xp,main="con 5 va")
hist(Xp,main="histograma con 30")
qqnorm(Xp, main="qqnorm xp con 30")
qqnorm(mat, main="qqnorm mat con 30")





Xp <- numeric(1000)
n <- 500
mat <- matrix(nrow=n,ncol = 1000)
for(i in 1:1000) {
	for(j in 1:n) {
		mat[j,i] <- rexp(n=1, rate=lambda)	
	}
}
Xp <- numeric(1000)
for(k in 1:1000){
	s <- sum(mat[,k])
	
	Xp[k] <- s/n
}
boxplot(Xp,main="con 5 va")
hist(Xp,main="histograma con 500")
qqnorm(Xp,main="qqnorm xp con 500")
qqnorm(mat, main="qnorm mat con 500")

boxplot(mat)
boxplot(Xp)