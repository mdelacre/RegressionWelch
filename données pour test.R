# Exemple pour vérifier mes calculs

Y <- c(-0.12911621,0.03892718,0.79808435,0.82563559,-0.17818144,-0.59742606,-1.21127987,0.24610344,-0.04301791,0.10571957,-0.38384048,1.34895385,0.11306734,-0.97393433,-0.66242339)
X <- c(-0.82530854,-0.05376678,-2.07173442,0.46077152,-0.83289799,-0.66773153,-0.02951330,2.97742312,-1.21891760,0.19849649,-1.13654004,0.91964779,0.98460346,-0.21175531,0.83098832)
n <- length(Y)
data <- data.frame(X,Y)

# r en fonction de t
t <- sqrt(summary(lm(Y~X))$fstatistic[1])
r <- t/sqrt(n+t^2-2) # = cor(Y,X,method="pearson"), parfait!

# t en fonction de r
r <- cor(Y,X,method="pearson")
t <- r/sqrt((1-r^2)/(n-2)) # = sqrt(summary(lm(Y~X))$fstatistic[1]), parfait!

b1 <- cov(Y,X)/var(X)
b1 <- t/sqrt(n+t^2-2)*(sd(Y)/sd(X)) # summary(lm(Y~X))$coefficients[2,1]

# Et si le prédicteur est catégoriel dichotomique? 
# La relation fonctionne toujours!

X <- c(rep(-1,9),rep(1,6))
data <- data.frame(X,Y)

t <- sqrt(summary(lm(Y~X))$fstatistic[1])
b1 <- t/sqrt(n+t^2-2)*(sd(Y)/sd(X)) # summary(lm(Y~X))$coefficients[2,1]


moy <- tapply(data[,2],data[,1],mean)
lambda <- as.numeric(names(table(X)))

b1 <- cov(Y,X)/var(X)
b1 <- (lambda[1]*moy[1]+lambda[2]*moy[2])/(lambda[1]^2+lambda[2]^2)