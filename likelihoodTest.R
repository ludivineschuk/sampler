## fonction de vraisemblance  ##
set.seed(0)
likelihood <- function (x) {
  2*x^2+6*x+3
}

#initialisation
position_theta <- 0
v1 <- likelihood (position_theta)
thetaSimul <- c()
v2Simul <- c()

# exploration
for (i in 1:50000) {
  pas <- rnorm(n=1, mean=0, sd=1)
  
  proposition_theta <- position_theta + pas
  v2 <- likelihood (proposition_theta)
  
  if (v2<v1){
    position_theta <- proposition_theta
    
    thetaSimul[i] <- position_theta
    v2Simul[i] <- v2
    
    print(paste("Abscisse :", position_theta, "Ordonnee :", v2 , "Accepté"))
  } else {
    print(paste("Abscisse :", position_theta, "Ordonnee :", v2 , " : Rejeté"))
  }
}

plot(x=-100:100,y=likelihood(-100:100))

# espérance theta et likelihood de theta
thetaMean <- mean(thetaSimul, na.rm=TRUE)
thetaMean
abline(v= thetaMean, col = "red")
LhMean <- mean(v2Simul, na.rm = TRUE)
abline(h = LhMean, col = "green")

plot(thetaSimul, type = "l", col = "orange")
hist(thetaSimul)
