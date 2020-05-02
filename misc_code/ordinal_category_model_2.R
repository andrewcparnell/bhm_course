# Code for plant endangeredness
# Similar to ordinal multinomial logit P119 (Gelman and Hill book)

# Simulate data -----------------------------------------------------------

# y_i is a value from 0 to 7
# plant growth form (pgf) 1 to 3 (herb, shrub and tree)
# country 1 to 100
# habitat 1 to 5
# use_type 1 to 3

N = 10000
pgf = sample(1:3, size = N, replace = TRUE)
country = sample(1:100, size = N, replace = TRUE)
habitat = sample(1:5, size = N, replace = TRUE)
use_type = sample(1:3, size = N, replace = TRUE)
sigma = 0.5
set.seed(123)
alpha_pgf = rnorm(3, 0, 1)
alpha_country = rnorm(100, 0, 1)
alpha_habitat = rnorm(5, 0, 1)
alpha_use_type = rnorm(3, 0, 1)
cuts = c(-1, -2/3, -1/3, 0, 1/3, 2/3, 1)
z = rnorm(N, alpha_pgf[pgf] + alpha_country[country] + alpha_habitat[habitat] + alpha_use_type[use_type], sigma)
y = findInterval(z, cuts)
data = data.frame(y, pgf, country, habitat, use_type)

# JAGS code ---------------------------------------------------------------

jags_code = '
model{
  # Likelihood
  for(i in 1:N) {
    z[i] ~ dnorm(alpha_pgf[pgf[i]] + alpha_country[country[i]] + alpha_habitat[habitat[i]] + alpha_use_type[use_type[i]], sigma^-2)
    y[i] ~ dinterval(z[i], cuts)
  }
  for(j in 1:N_pgf) {
    alpha_pgf[j] ~ dnorm(mu_all, sigma_pgf^-2)
  }
  for(j in 1:N_country) {
    alpha_country[j] ~ dnorm(0, sigma_country^-2)
  }
  for(j in 1:N_habitat) {
    alpha_habitat[j] ~ dnorm(0, sigma_habitat^-2)
  }
  for(j in 1:N_use_type) {
    alpha_use_type[j] ~ dnorm(0, sigma_use_type^-2)
  }

  mu_all ~ dnorm(0, 10^-2)
  sigma_pgf ~ dt(0, 10, 1)T(0, )
  sigma_country ~ dt(0, 10^-2, 1)T(0, )
  sigma_habitat ~ dt(0, 10^-2, 1)T(0, )
  sigma_use_type ~ dt(0, 10^-2, 1)T(0, )
  sigma ~ dt(0, 10^-2, 1)T(0, )
}
'

# JAGS run ----------------------------------------------------------------

jags_inits = function() {
  z = runif(N, -1.33, -1)
  z[y==1] = runif(sum(y==1), -1, -2/3)
  z[y==2] = runif(sum(y==2), -2/3, -1/3)
  z[y==3] = runif(sum(y==3), -1/3, 0)
  z[y==4] = runif(sum(y==4), 0, 1/3)
  z[y==5] = runif(sum(y==5), 1/3, 2/3)
  z[y==6] = runif(sum(y==6), 2/3, 1)
  z[y==7] = runif(sum(y==7), 1, 4/3)
  return(list(z = z))
}
jags_run = jags(data = list(N = N,
                            y = y,
                            pgf = pgf,
                            country = country,
                            use_type = use_type,
                            habitat = habitat,
                            N_pgf = 3,
                            N_country = 100,
                            N_use_type = 3,
                            N_habitat = 5,
                            cuts = cuts),
                inits = jags_inits,
                n.iter = 10000,
                n.burnin = 2000,
                n.thin = 8,
                parameters.to.save = c('alpha_pgf',
                                       'alpha_country',
                                       'alpha_habitat',
                                       'alpha_use_type',
                                       'mu_all',
                                       'sigma_pgf',
                                       'sigma_country',
                                       'sigma_habitat',
                                       'sigma_use_type',
                                       'sigma'),
                model.file = textConnection(jags_code))


# Results -----------------------------------------------------------------

plot(jags_run)
print(jags_run)
