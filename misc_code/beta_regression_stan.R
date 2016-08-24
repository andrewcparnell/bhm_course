# Beta regression example

# y[i] ~ beta(alpha[i], beta[i])

# Set m[i] = alpha[i] / (alpha[i] + beta[i])
# Set s[i] = alpha[i] + beta[i]

# Code taken from https://github.com/m-clark/Miscellaneous-R-Code/blob/master/ModelFitting/Bayesian/rstanBetaRegression.R

stan_code = '
data {
  int<lower=1> N;                   // sample size
  vector<lower=0,upper=1>[N] y;     // response 
  vector[N] x1; // Explanatory variable 1
  vector[N] x2; // Explanatory variable 2
}
parameters {
  real alpha; // Intercept
  real beta_1;                   // reg coefficients
  real beta_2;                   // reg coefficients
  real<lower=0> phi;                // dispersion parameter
}
model {
  // model calculations
  vector[N] mu;    // transformed linear predictor
  vector[N] A;             // parameter for beta distn
  vector[N] B;             // parameter for beta distn

  for (i in 1:N) { 
    mu[i] = inv_logit(alpha + beta_1 * x1[i] + beta_2 * x2[i]);   
  }
  A = mu * phi;
  B = (1.0 - mu) * phi;
  // priors
  alpha ~ normal(0, 10);   
  beta_1 ~ normal(0, 10);   
  beta_2 ~ normal(0, 10);   
  phi ~ cauchy(0, 5);               // different options for phi  

  // likelihood
  y ~ beta(A, B);
}
'


# Simulate data -----------------------------------------------------------

set.seed(1234)

N = 500
x1 = sort(rnorm(N))
x2 = sort(rnorm(N))
X = cbind(1, x1, x2)
beta = c(-1, 0.2, -0.3)
mu = inv.logit(X%*%beta)  # add noise if desired + rnorm(N, sd=.01)
phi = 10
A = mu*phi
B = (1-mu)*phi
y = rbeta(N, A, B)
hist(y, breaks = 30)


# Run the model -----------------------------------------------------------

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
stan_run = stan(data = list(N = N,
                            x1 = x1,
                            x2 = x2,
                            y = y),
                model_code = stan_code)
plot(stan_run)
print(stan_run)
