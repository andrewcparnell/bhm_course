# An example of a better posterior predictive plot

# Load in package
library(R2jags)

# Load in data and check
swt = read.csv('../data/swt.csv', stringsAsFactors = TRUE) # Might need to
head(swt)

# JAGS code for a binomial hierarchical model
jags_code = '
model{
  # Likelihood
  for(i in 1:N) {
    y[i] ~ dbin(p[i], N_exp[i])
    y_pp[i] ~ dbin(p[i], N_exp[i])
    logit(p[i]) <- alpha[alt[i]] + beta[alt[i]]* (x[i] - mean(x))
  }
  # Priors
  for(j in 1:N_alt) {
    alpha[j] ~ dnorm(mu_alpha, sigma_alpha^-2)
    beta[j] ~ dnorm(mu_beta, sigma_beta^-2)
  }
  mu_alpha ~ dnorm(0, 5^-2)
  mu_beta ~ dnorm(0, 0.1^-2)
  sigma_alpha ~ dt(0, 5^-2, 1)T(0,)
  sigma_beta ~ dt(0, 5^-2, 1)T(0,)
}
'

#Get the correct data set by ignorinsg the NA values
sum_fun = function(x) {
  s = ifelse(is.na(x[1]),0,x[1]) + ifelse(is.na(x[2]),0,x[2]) + ifelse(is.na(x[3]),0,x[3])
  N = ifelse(is.na(x[1]),0,1) + ifelse(is.na(x[2]),0,1) + ifelse(is.na(x[3]),0,1)
  return(c(s,N))
}
y = apply(swt[,1:3],1,sum_fun)[1,]
N = apply(swt[,1:3],1,sum_fun)[2,]

# Run the model on the correct data
jags_run = jags(data = list(N = nrow(swt),
                            N_exp = N,
                            N_alt = length(unique(swt$alt)),
                            alt = swt$alt,
                            y = y,
                            x = swt$forest),
                parameters.to.save = c('y_pp',
                                       'alpha',
                                       'beta',
                                       'mu_alpha',
                                       'mu_beta',
                                       'sigma_alpha',
                                       'sigma_beta'),
                model.file = textConnection(jags_code))

# Produce the posterior predictive plot
y_pp = jags_run$BUGSoutput$mean$y_pp

# In the main the y values agree iwth teh posterior predictive values
plot(jitter(y, 0.1), y_pp, xlim = range(y), ylim = range(y), xlab = 'y', ylab = 'y_pp')
abline(a=0, b = 1, col = 'red')
