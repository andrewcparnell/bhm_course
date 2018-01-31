# Header ------------------------------------------------------------------

# Fitting a logistic regression in Stan
# Andrew Parnell

# In this file we fit a Bayesian Generalised Linear Model (GLM) in the form
# of a logistic regression.

# Some boiler plate code to clear the workspace, and load in required packages
rm(list=ls()) # Clear the workspace
library(boot)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# Maths -------------------------------------------------------------------

# Description of the Bayesian model fitted in this file
# Notation:
# y_t = binomial (often binary) response variable for observation t=1,...,N
# x_{1t} = first explanatory variable for observation t
# x_{2t} = second " " " " " " " " "
# p_t = probability of y_t being 1 for observation t
# alpha = intercept term
# beta_1 = parameter value for explanatory variable 1
# beta_2 = parameter value for explanatory variable 2

# Likelihood
# y_t ~ Binomial(K,p_t), or Binomial(1,p_t) if binary
# logit(p_t) = alpha + beta_1 * x_1[t] + beta_2 * x_2[t]
# where logit(p_i) = log( p_t / (1 - p_t ))
# Note that p_t has to be between 0 and 1, but logit(p_t) has no limits

# Priors - all vague
# alpha ~ normal(0,100)
# beta_1 ~ normal(0,100)
# beta_2 ~ normal(0,100)

# Simulate data -----------------------------------------------------------

# Some R code to simulate data from the above model
N = 100
set.seed(123)
x_1 = sort(runif(N,0,10))
x_2 = sort(runif(N,0,10))
alpha = 1
beta_1 = 0.2
beta_2 = -0.5
logit_p = alpha + beta_1 * x_1 + beta_2 * x_2
p = inv.logit(logit_p)
y = rbinom(N,1,p)

# Have a quick look at the effect of x_1 and x_2 on y
plot(x_1,y)
plot(x_2,y) # Clearly when x is high y tends to be 0

# Stan code ---------------------------------------------------------------

# Stan code to fit the model to the simulated data
model_code = '
data {
  int<lower=0> N; // Number of observations
  int<lower=0> K; // Number of explanatory variables
  matrix[N, K] x; // Matrix of explanatory variables
  int<lower=0,upper=1> y[N];
}
parameters {
  real alpha;
  vector[K] beta;
}
model {
  y ~ bernoulli_logit(x * beta);
}
'

stan_run = stan(data = list(N = N,
                            K = 3,
                            y = y,
                            x = cbind(1, x_1, x_2)),
                model_code= model_code)

# Simulated results -------------------------------------------------------

plot(stan_run)
print(stan_run)

