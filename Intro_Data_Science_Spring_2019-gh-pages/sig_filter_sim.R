#Set up our hypothetical experiment as in the slides
mu <- 2 #The true effect (not observed in practice)
se <- 1 #The standard error of the estimates we produce
alpha <- 0.05 #The significance level we will use
z_alpha <- qnorm(1-alpha) #The value that our estimate must be larger than to have p-value less than alpha

#Simulate this experiment once
z <- rnorm(1,2,1)

#Simulate this experiment many times and track the statistically significant results
S <- 10000 #Number of simulations to run
effs <- rnorm(S,mu,se) #List of our effect estimates
sig <- effs > z_alpha #List to indicate which simulated effects are statistically significant

hist(effs,main="All Effect Estimates") #Histogram of effect estimates - follows the normal(2,1) distribution closely
mean(effs) #And the average effect estimate is close to the true effect

hist(effs[sig],main="Statistically Significant Effect Estimates") #Histogram of significant effects - looks like normal(2,1) truncated below z_alpha
abline(v=z_alpha,col='red',lwd=2)
mean(effs[sig]) #Average of statistically significant effect estimates larger than true effect
mean(effs[sig])/mu #Compute ratio of average significant estimate and truth to quantify exaggeration 

#Now adjust the significance level to see how it relates to the exaggeration factor
alphas <- c(0.1,0.05,0.01,0.005,0.001,0.0005,0.0001)
z_alphas <- qnorm(1-alphas)
S <- 100000
exs <- numeric(length=length(alphas)) #List to store exaggeration ratio for each alpha level

#Simulate same process as above for each value of alpha and store exaggeration ratio
for(i in 1:length(alphas)){
  effs <- rnorm(S,mu,se)
  sig <- effs > z_alphas[i]
  exs[i] <- mean(effs[sig])/mu
}

#Plot exaggeration ratios against negative log of each alpha
#Taking log makes visualization easier
log_alphas <- -1*log(alphas)
plot(exs~log_alphas,col='red',lwd=2,main="Exaggeration ratio against significance level on log scale",xlab="-log alpha",ylab="exaggeration ratio")
lines(exs~log_alphas,col='red',lwd=2)

######################################################################################################################################################

#Now we can examine the effect of this over-estimation on power calculations
#First, what is our true power using the same parameters as above?
effs <- rnorm(S,mu,se)
alpha <- 0.01
z_alpha <- qnorm(1-alpha)
sig <- effs > z_alpha
mean(sig) #Compute the proportion of those effect estimates which are statistically significant - this is close to true power

#Define a function to compute power given significance level, data standard deviation, estimated effect size, and sample size
power <- function(alpha,mu_est,sigma,N){
  beta <- 1-pnorm(qnorm(1-alpha)-(mu_est/(sigma/sqrt(N))))
  return(beta)
}

#Compute power with this function and check that it agrees with the simulated power above
N <- 25 #Set sample size to 25
sigma <- 5 #Set sample standard deviation to 
mu_est <- 2 #Suppose first our estimated effect size is right
power(alpha,2,sigma,N)

#Define a function to compute the effective sample size from desired power, the significance level, the effect size estimate, and the data standard deviation
samp_size <- function(beta,alpha,mu_est,sigma){
  sqrt_N <- (qnorm(1-alpha)-qnorm(1-beta))*(sigma/mu_est)
  N <- sqrt_N^2
  return(N)
}

#Check that if we use the power we got above, we get back the sample size that we used
samp_size(0.372,alpha,mu_est,sigma)

#Now take the average of the statistically significant results as our effect estimate
mu_est <- mean(effs[sig])
power_target <- 0.8 #Set our target power to 80% - we will use this and eff_est to get a sample size
N <- samp_size(power_target,alpha,mu_est,sigma) #Compute the sample size we think we will need using our effect size estimate
actual_pow <- power(alpha,2,sigma,N) #Use the true effect size to compute the actual power of this study
actual_pow/power #Check the ratio of the actual power to the power we intended
