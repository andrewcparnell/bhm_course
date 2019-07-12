# Try and build a model to estimate total population size of dogs based on multiple surveys

# Clear workspace and load in packages
rm(list = ls())
library(R2jags)
library(boot)

# Best estimate of total number of dogs to start with is 9.5million
# Let's suppose there are 4 post-codes and 2 different surveys
# i is postcode (1 to 4) and j is survey (1 to 2)
# Let N_i be the total population size in each post-code so that
# N = sum_i N_i
# Let n_{ij} the total number of observed dogs in postcode i in survey j
# Let p_{ij} be the probability that you observe dogs in post-code i in survey j
# Let lambda be the overall mean number of dogs across post-codes

# The model is:
# n_{ij} ~ dbin(N_i, p_{ij})
# N_i ~ dpois(lambda)
# N = \sum_i N_i

# Simulate some data
set.seed(123)
I = 4 # postcodes
J = 2 # surveys
lambda = 2*10^6 # Big number
N_i = rpois(I, lambda) # Population in each post-code
N = sum(N_i)
x = sort(runif(I)) # A covariate that affects the probabilities of observing the dogs in a survey
p_ij = matrix(NA, nrow = I, ncol = J) # Probability of observing dogs by postcode (row) and survey (column)
for (i in 1:I) {
  for (j in 1:J) {
    p_ij[i,j] = inv.logit(-0.2 + ifelse(j==1,-2,2) * x[i])
  }
}

n_ij = matrix(NA, nrow = I, ncol = J)
for(i in 1:I) {
  for(j in 1:J) {
    n_ij[i,j] = rbinom(1, N_i[i], p_ij[i,j])
  }
}

model_code = '
model {
  for(i in 1:I) { # Loop through postcode
    for(j in 1:J) { # Loop through survey
      n_ij[i,j] ~ dbinom(p_ij[i,j], N_i[i])
      logit(p_ij[i,j]) = alpha[j] + beta[j] * (x[i] - mean(x))
    }
    N_i[i] ~ dpois(lambda) # lambda is the prior estimate of number of dogs in this postcode
  }
  for (j in 1:J) {
    alpha[j] ~ dnorm(0, 1)
    beta[j] ~ dnorm(0, 1)
  }

  lambda ~ dnorm(2000000, 100000^-2) # Prior on number of dogs in postcodes
  N = sum(N_i) # Sum up all the populations in each postcode to give total estimate
}
'

jags_run = jags(data = list(I = I, # The number of post-codes
                            J = J, # The number of surveys
                            x = x, # a covariate such as population density
                            n_ij = n_ij), # Counts of dogs in each post code and each survey
                parameters.to.save = c('N', 'N_i', 'lambda'),
                model.file = textConnection(model_code))

plot(jags_run)
hist(jags_run$BUGSoutput$sims.list$N, breaks = 30)
abline(v = N, col = 'red')
