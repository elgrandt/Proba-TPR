lambda <- 1/4
for (i in 1:3000){
  set.seed(1)
  muestra <- rexp(n=i,rate=lambda)
  Xn[i] <- mean(muestra) 
}
plot(Xn, main="Promedios", xlab = "N", ylab = "Promedio hasta N")