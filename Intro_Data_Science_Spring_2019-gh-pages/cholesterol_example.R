#You will need to have rstan installed to run much of this code
library(rstan)
library(ggplot)
library(cowplot)

#Generate fake data for our model
alpha <- 220
beta <- 20
alphas <- rnorm(200,alpha,20)
betas <- rnorm(100,beta,5)
y <- matrix(nrow=200,ncol=2)
y[,1] <- alphas + rnorm(200,0,2)
y[1:100,2] <- alphas[1:100] + rnorm(100,0,2)
y[101:200,2] <- alphas[101:200] + betas + rnorm(100,0,2)
treat <- c(rep(0,100),rep(1,100))

#Format the data for stan, and compile and fit the first model
study_data <- list(y0 = y[,1], y1 = y[,2], N = nrow(y), treat = treat)
model_1 <- stan_model("cholesterol_model_1.stan")
fit_1 <- sampling(model_1,data=study_data)

#Extract posterior samples and computed errors from the first model
par_fit <- extract(fit_1,pars=c('alpha','beta'))
err_fit <- extract(fit_1,pars=c('err0','err1'))

#Plot the posterior samples of alpha and beta
ggplot(data=data.frame(x=par_fit$alpha),aes(x)) + geom_histogram()
ggplot(data=data.frame(x=par_fit$beta),aes(x)) + geom_histogram()

#Plot the epsilon values for a handful of simulations
plots <- as.list(vector(length=12))
for (i in 1:12){
  err0 <- err_fit$err0[i,]
  err1 <- err_fit$err1[i,]
  plots[[i]] <- ggplot(data=data.frame(x=err0,y=err1),aes(x,y)) +
    geom_point() +
    xlab("epsilon_0") + 
    ylab("epsilon_1")
}
plot_grid(plotlist=plots)

#Compile and fit the second model
model_2 <- stan_model("cholesterol_model_2.stan")
fit_2 <- sampling(model_2,data=study_data)

#Extract posterior samples and computed errors from the second model
par_fit <- extract(fit_2,pars=c('alpha','beta'))
err_fit <- extract(fit_2,pars=c('err0','err1'))

#Plot the posterior samples of alpha and beta
ggplot(data=data.frame(x=par_fit$alpha),aes(x)) + geom_histogram()
ggplot(data=data.frame(x=par_fit$beta),aes(x)) + geom_histogram()

#Plot the epsilon values for a handful of simulations
plots <- as.list(vector(length=12))
for (i in 1:12){
  err0 <- err_fit$err0[i,]
  err1 <- err_fit$err1[i,]
  plots[[i]] <- ggplot(data=data.frame(x=err0,y=err1),aes(x,y)) + 
    geom_point() +
    xlab("epsilon_0") + 
    ylab("epsilon_1")
}
plot_grid(plotlist=plots)
