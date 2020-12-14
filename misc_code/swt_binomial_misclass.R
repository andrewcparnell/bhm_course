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
    y[i] ~ dbin(p[i], 1)
    y_pp[i] ~ dbin(p[i], 1)
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


# Run the model on the correct data
jags_run = jags(data = list(N = nrow(swt),
                            N_alt = length(unique(swt$alt)),
                            alt = swt$alt,
                            y = swt$rep.1,
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

# In the main the y values agree with the posterior predictive values
plot(jitter(swt$rep.1, 0.2), y_pp, xlim = range(swt$rep.1), ylim = range(swt$rep.1), xlab = 'y', ylab = 'y_pp')
abline(a=0, b = 1, col = 'red')

y_pp2 = jags_run$BUGSoutput$median$y_pp
table(swt$rep.1, y_pp2)
