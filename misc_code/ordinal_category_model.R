# Code for Latent kids scores
# Similar to ordinal multinomial logit P119 (Gelman and Hill book)



# Simulate data -----------------------------------------------------------

# y_i is a value from 0 to 4
# id which is a kid id values from 1 to 10
# test is a value from 1 to 2

N = 20
test = rep(1:2,length=N)
id = sort(rep(1:10, length = N))
sigma = 0.5
set.seed(123)
alpha_id = rnorm(10, 0, 1)
alpha_test = rnorm(2, 0, 1)
cuts = c(-0.75, -0.25, 0.25, 0.75)
z = rnorm(N, alpha_test[test] + alpha_id[id], sigma)
y = findInterval(z, cuts)
data = data.frame(y, id, test)

# JAGS code ---------------------------------------------------------------

jags_code = '
model{
  # Likelihood
  for(i in 1:N) {
    z[i] ~ dnorm(alpha_test[test[i]] + alpha_id[id[i]], sigma^-2)
    y[i] ~ dinterval(z[i], cuts)
  }
  for(j in 1:N_test) {
    alpha_test[j] ~ dnorm(mu_all, sigma_test^-2)
  }
  test_diff = alpha_test[2] - alpha_test[1]

  for(k in 1:N_id) {
    alpha_id[k] ~ dnorm(0, sigma_id^-2)
  }
  mu_all ~ dnorm(0, 10^-2)
  sigma_test ~ dt(0, 10^-2, 1)T(0, )
  sigma_id ~ dt(0, 10^-2, 1)T(0, )
  sigma ~ dt(0, 10^-2, 1)T(0, )
}
'

# JAGS run ----------------------------------------------------------------

jags_inits = function() {
  z = runif(N, -1.25, -0.75)
  z[y==1] = runif(sum(y==1), -0.75, -0.25)
  z[y==2] = runif(sum(y==2), -0.25, 0.25)
  z[y==3] = runif(sum(y==3), 0.25, 0.75)
  z[y==4] = runif(sum(y==4), 0.75, 1.25)
  return(list(z = z))
}
jags_run = jags(data = list(N = N,
                            y = y,
                            test = test,
                            id = id,
                            N_id = 10,
                            N_test = 2,
                            cuts = cuts),
                inits = jags_inits,
                n.iter = 10000,
                n.burnin = 2000,
                n.thin = 8,
                parameters.to.save = c('alpha_test',
                                       'alpha_id',
                                       'mu_all',
                                       'sigma_test',
                                       'test_diff',
                                       'sigma_id',
                                       'sigma'),
                model.file = textConnection(jags_code))


# Results -----------------------------------------------------------------

plot(jags_run)
print(jags_run)
