#Creating vectors
Y<-rep(1,816)
Y <- append(Y,rep(0,3203))
Y <- append(Y,rep(1,188))
Y <- append(Y,rep(0,1168))
X<-rep(1,4019)
X <- append(X,rep(0,1356))

#Creatimg Dataset
smokers.dataset <- data.frame(Y,X)


#Fitting Logistic regression model
smokers.classical <- glm(Y~X, data = smokers.dataset, family = binomial())



