data{
  int N;  //Number of individuals in the study
  real y0[N];  //Pre-treatment cholesterol measurements
  real y1[N];  //Post-treatment cholesterol measurements
  int<lower=0,upper=1> treat[N];  //Treatment indicator variables
}

parameters{
  real alpha;  //Average baseline cholesterol 
  real beta;  //Average treatment effect
  real<lower=0> sigma;  //Variance of random errors
}

transformed parameters{
  real pre_mean[N];  //Predicted pre-treatment cholesterol
  real post_mean[N];  //Predicted post-treatment cholesterol
  for (i in 1:N){
    pre_mean[i] = alpha;
    post_mean[i] = alpha + beta*treat[i];
  }
}

model{
  y0 ~ normal(pre_mean,sigma);  //Pre-treatment cholesterol sampling distribution
  y1 ~ normal(post_mean,sigma);  //Post-treatment cholesterol sampling distribution
  alpha ~ normal(230,10);  //Average baseline cholesterol prior
  beta ~ normal(15,10);  //Average treatment effect prior
  sigma ~ exponential(inv(3));  //Random error variance prior
}

generated quantities{
  real err0[N];  //Random error of pre-treatment measurement
  real err1[N];  //Random error of post-treatment measurement
  for (i in 1:N){
    err0[i] = y0[i] - pre_mean[i];
    err1[i] = y1[i] - post_mean[i];
  }
}
