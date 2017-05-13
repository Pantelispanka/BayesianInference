#The Data of the problem
data <- c(60, 119, 100, 130, 43, 227, 23, 91, 128, 199, 85, 125, 40, 26, 141, 212, 238, 94, 111, 67)



#Creating x values
x <- seq(1,800,by=1)

#Plotting Prior Distribution (truncated Normal N(0,1000) in 0-500)
plot(x, dtnorm(x,0,1000, 0 , 500), type = "l", ylim = c(0,0.004))

#Adding the Posterior with mean = 95.573, variance = 153.8462 As calculated above
lines(x, dtnorm(x,95.573,153.8462, 0 , 500), type = "l")

#Likelihood function of normal dist with known variance
likelihood.normal.mu <- function(mu, sig2, x) {
     # mu mean of normal distribution for given sig
     # sig2 variance of normall distribution
     # x vector of data
     n = length(x)
     a1 = (2*pi*sig2)^-(n/2)
     a2 = -1/(2*sig2)
     y = (x-mu)^2
     ans = a1*exp(a2*sum(y))
     return(ans) 
 }

#Intitialize new empty array for the results of the likelihood
likelihood_results <- c()

#Gathering results from the likelihood estimator
for (i in 1:length(x)){ likelihood_results[i] <- likelihood.normal.mu(mean(data), sig2=3600, x = x[i])}

#Plotting Likelihood , Posterior, Prior
plot(x, dtnorm(x,0,1000, 0 , 500), type = "l", ylim = c(0,0.01), col = "brown")
lines(x, dtnorm(x,95.573,153.8462, 0 , 500), type = "l")
lines(x, likelihood_results, type = "l", col = "red")

#Adding text for better visualization
text(x=600,y=0.0085, label = "Likelihood estimation", col = "red")
text(x=600,y=0.0078, label = "Posterior")
text(x=600,y=0.0071, label = "Prior", col = "brown")







