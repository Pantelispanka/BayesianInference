metropolis.hastings <- function(iter, sigma){
     accept <- 0
     met <- numeric(iter)
     last<- 120
     sd <- sigma^2
     for(i in 1:iter){
         cand <- rnorm(1,last,sigma)
         alpha <- (likelihood.normal.mu(cand,sd,data)*rnorm(1,cand,sigma))/(likelihood.normal.mu(last,sd,data)*rnorm(1,last,sigma))
         if(runif(1) < min(alpha,1)){
             last<-cand
             accept <- accept+1
         }
         met[i] <- last
     }
     list(mean=met,accept=accept/iter)
}

metropolis.run.1 <- metropolis.hastings(5000, 10)
metropolis.run.2 <- metropolis.hastings(5000, 20)
metropolis.run.3 <- metropolis.hastings(5000, 30)
metropolis.run.4 <- metropolis.hastings(5000, 40)
metropolis.run.5 <- metropolis.hastings(5000, 50)
metropolis.run.6 <- metropolis.hastings(5000, 80)


#Histogram of posterior values
posterior.results <- c()
posterior.results <- rtnorm(x,95.573,sqrt(153.8462))
hist(posterior.results)


#Plotting  Metropolis Hastings values
plot(metropolis.run.4$mean,type = "l")

hist(metropolis.run.4$mean[1000:5000])

#Computinh mean and variance of MCMC values
mean(metropolis.run.4$mean[1000:5000])
var(metropolis.run.4$mean[1000:5000])

#Comparing Theoritical / MCMC values
plot(x, dtnorm(x,95.573,sqrt(153.8462), 0 , 500), type = "l", ylim = c(0,0.04))
lines(density(metropolis.run.5$mean), col="red")
text(x=600,y=0.035, label = "MCMC values", col = "red")
text(x=600,y=0.032, label = "theoretical Posterior")
#Autocorellation plot
acf(metropolis.run.5$mean)

# compute ergodic mean
erg.mean<-function(x){  
  n<-length(x)
  result<-cumsum(x)/cumsum(rep(1,n))
}

plot(erg.mean(metropolis.run.4$mean), type = "l")


#right values
plot(x, dtnorm(x,112.573,12.36932, 0 , 500), type = "l", ylim = c(0,0.04))
lines(density(metropolis.run.5$mean), col="red")
text(x=600,y=0.035, label = "MCMC values", col = "red")
text(x=600,y=0.032, label = "theoretical Posterior")




#Metropolis Hastings with truncated normal
metropolis.hastings.trun <- function(iter, sigma){
     accept <- 0
     met <- numeric(iter)
     last<- 120
     sd <- sigma^2
     for(i in 1:iter){
         cand <- rtnorm(1,last,sigma, 0,500)
         alpha <- (likelihood.normal.mu(cand,sd,data)*rtnorm(1,cand,sigma, 0,500))/(likelihood.normal.mu(last,sd,data)*rtnorm(1,last,sigma, 0,500))
         if(runif(1) < min(alpha,1)){
            last<-cand
            accept <- accept+1
         }
         met[i] <- last
     }
    list(mean=met,accept=accept/iter)
}






