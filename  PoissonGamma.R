#The data of the problem
ship.data <- c(9,4,5,5,7,13,8,3,6,5,4,5,10,5,5,4,3,3,4,7)


#Ploting poisson pdf for 5.75 lambda
plot(dpois(1:40,5.75), type = "l", ylim = c(0,0.3))





#Poisson likelihood
poisson.likelihood <- function(lamda,data){
  exp(-lamda)*sum( (lamda^data)/factorial(data) )
}

#Empty plot to add intervals
plot(NULL, xlim=c(0,50), ylim=c(0,60), ylab="Θ values", xlab="Confidence inerval")



#Confidence interval for ship data
for(i in 1:length(ship.data)){
  conf.int <- poisson.test(ship.data[i], conf.level = 0.95)
  segments(x0 = conf.int$conf.int[1], y0 = ship.data[i], x1 = conf.int$conf.int[2], y1 = ship.data[i])
}


#Confidence interval for lambda 1 to 40
for(i in 1:40){
  conf.int <- poisson.test(i, conf.level = 0.95)
  segments(x0 = conf.int$conf.int[1], y0 = i, x1 = conf.int$conf.int[2], y1 = i)
}

#Adding estimated lambda value
segments(x0 = 5.75, y0 = 0, x1 = 5.75, y1 = 40)


#Gennerating the posterior function
posterior <- function(xnew,data,a,b){
  (gamma(sum(data)+a+xnew) / (gamma(sum(data + a))*gamma(xnew + 1) )) * ((length(data)+b)/(length(data) + b + 1))^(sum(data)+a) *(1/ (length(data) + b + 1)^xnew)
}


#Plot both Likelihood estimation of λ and posterior
plot(dpois(1:40,5.75), type = "l", ylim = c(0,0.3), col="red")
lines(1:40, posterior(1:40,ship.data,0.001,0.001), type = "l", col = "blue")
text(x=25,y=0.25, label = "Maximum likelihood estimation of Poisson", col = "red")
text(x=25,y=0.22, label = "Posterior function", col ="blue")


#Clear Plot of the posterior
plot(seq(1,40, by=0.1), posterior(seq(1,40, by=0.1),ship.data,0.001,0.001), type = "l", col = "blue", xlab = "λ")

