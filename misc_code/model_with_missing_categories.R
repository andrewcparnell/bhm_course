# A model with missing categories

# Clear the workspace
rm(list=ls())

# Load in packages
library(R2jags)

# Simulate some data ------------------------------------------------------

N = 100
cat1 = sample(1:3, size = N, replace = TRUE) # 4th level is always 0
N_1 = 50
cat1[(N_1+1):N] = 4
cat2 = sample(1:4, size = N, replace = TRUE) # 5th level is always 0
cat2[1:N_1] = 5
set.seed(100)
mu_all = 2
alpha = c(rnorm(3, 0, 1),0)
beta = c(rnorm(4, 0, 1),0)
sigma = 0.1
y = rnorm(N, mean = mu_all + alpha[cat1] + beta[cat2], sd = sigma)
data = cbind(y, cat1, cat2)

# JAGS_code ---------------------------------------------------------------

jags_code = '
model{
  # Likelihood
  for(i in 1:N) {
    y[i] ~ dnorm(mu_all + alpha[cat1[i]] + beta[cat2[i]] , sigma^-2)
  }
  for(j in 1:N_cat1) {
    alpha[j] ~ dnorm(0, sigma_cat1^-2)  
  }
  alpha[N_cat1 + 1] <- 0
  for(j in 1:N_cat2) {
    beta[j] ~ dnorm(0, sigma_cat2^-2)
  }
  beta[N_cat2 + 1] <- 0

  mu_all ~ dnorm(0, 10^-2)
  sigma_cat1 ~ dt(0, 10, 1)T(0, )
  sigma_cat2 ~ dt(0, 10, 1)T(0, )
  sigma ~ dt(0, 10, 1)T(0, )
}
'

# JAGS run ----------------------------------------------------------------

jags_run = jags(data = list(N = N,
                            N_cat1 = 3,
                            N_cat2 = 4,
                            y = y,
                            cat1 = cat1,
                            cat2 = cat2),
                parameters.to.save = c('alpha',
                                       'beta',
                                       'mu_all',
                                       'sigma_cat1',
                                       'sigma_cat2',
                                       'sigma'),
                model.file = textConnection(jags_code))


# Results -----------------------------------------------------------------

plot(jags_run)
print(jags_run)

