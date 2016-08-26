# Simple spatial hierarchical model in JAGS

# Clear the workspace
rm(list=ls())

# Load in packages
library(R2jags)
library(fields)
library(mvtnorm)

# Simulate data -----------------------------------------------------------

N = 200
alpha = 2
beta = 0.3
sigma = 2
tau = 0.4
rho = 1
lat = rnorm(N, 0, 1)
long = rnorm(N, 0, 1)
plot(lat, long)
d = rdist(cbind(lat,long))
Sigma = matrix(NA, N, N)
Mu = rep(NA, N)
pow = function(x,p) x^p
for(i in 1:N) {
  Mu[i] <- 0
  for(j in 1:N) {
    Sigma[i,j] <- pow(tau, 2) * exp( - pow(d[i,j], 2) / pow(rho, 2) )
  }
  Sigma[i,i] <- pow(sigma, 2) + pow(tau, 2)
}
s = rmvnorm(1,Mu,Sigma)
x = sort(rnorm(N, 0, 1))
y = rnorm(N, alpha + beta * x + s, sigma)

# JAGS code ---------------------------------------------------------------

jags_code = '
model
{
  # Likelihood
  for(i in 1:N) {
    y[i] ~ dnorm(alpha + beta*(x[i] - mean(x)) + s[i], sigma^-2)
  }

  # s is the spatial random effect
  s ~ dmnorm(Mu, Sigma.inv)
  Sigma.inv <- inverse(Sigma)
  
  # Set up mean and covariance matrix
  for(i in 1:N) {
    Mu[i] <- 0
    Sigma[i,i] <- pow(sigma, 2) + pow(tau, 2)
  
    for(j in (i+1):N) {
      Sigma[i,j] <- pow(tau, 2) * exp( - pow(d[i,j], 2) / pow(rho, 2) )
      Sigma[j,i] <- Sigma[i,j]
    }
  }
  
  # Priors
  alpha ~ dnorm(0, 100^-2)
  beta ~ dnorm(0, 100^-2)
  sigma ~ dunif(0, 100)
  tau ~ dunif(0, 10)
  rho ~ dunif(1, 100)

} 
'

# JAGS run ----------------------------------------------------------------

jags_run = jags(data = list(N = N,
                            y = y,
                            x = x,
                            d = d),
                   parameters.to.save = c('alpha',
                                          'beta',
                                          'tau',
                                          'sigma',
                                          'rho'),
                   model.file=textConnection(jags_code))
plot(jags_run)
