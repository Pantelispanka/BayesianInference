#Generating the dataset
Y <- rep(0,10)
Y <- append(Y,rep(1,4))
Y <- append(Y,rep(0,30))
Y <- append(Y,rep(1,4))
Y <- append(Y,rep(0,32))
Y <- append(Y,rep(1,2))
X <- rep(0,14)
X <- append(X,rep(1,34))
X <- append(X,rep(2,34))


rats.dataframe <- data.frame(Y,X)

#Running a GLM 
classical.log <- glm(Y~X, data = rats.dataframe, family = binomial())
summary(classical.log)


#Non Inf Beta
x.beta <- seq(0.01,1,by=0.01)
plot(x.beta, dbeta(x.beta, 0.5,0.5))


#Inf Beta
plot(x.beta, dbeta(x.beta, 263, 1461), type = "l")


