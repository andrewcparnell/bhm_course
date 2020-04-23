# Code taken from class 3 and 4

par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=-.01,las=1)

# First class 3 -----------------------------------------------------------

dat = read.csv('data/earnings.csv')
with(dat, plot(height_cm, log(earn)))


## ------------------------------------------------------------------------
model = lm(log(earn) ~ height_cm, data = dat)
summary(model)


## ---- echo=FALSE, results='hide', message=FALSE--------------------------
library(R2jags)
model_code ='
model {
  for(i in 1:N) {
    y[i] ~ dnorm(intercept + slope*x[i], residual_sd^-2)
  }
  intercept ~ dnorm(0,100^-2)
  slope ~ dnorm(0,100^-2)
  residual_sd ~ dunif(0,100)
}
'
model_parameters =  c("intercept", "slope", "residual_sd")
model_run = jags(data = list(N = nrow(dat),
                             y = log(dat$earn),
                             x = dat$height_cm),
                 parameters.to.save = model_parameters,
                 model.file = textConnection(model_code))


## ------------------------------------------------------------------------
print(model_run)


## ---- echo=FALSE, results='hide', message=FALSE--------------------------
library(R2jags)
model_code ='
model {
  for(i in 1:N) {
    y[i] ~ dnorm(intercept + slope*x[i], residual_sd^-2)
  }
  intercept ~ dnorm(9,0.5^-2)
  slope ~ dnorm(0,100^-2)
  residual_sd ~ dunif(0,100)
}
'
model_parameters =  c("intercept", "slope", "residual_sd")
model_run_2 = jags(data = list(N=nrow(dat),
                               y = log(dat$earn),
                               x = dat$height_cm),
                   parameters.to.save = model_parameters,
                   model.file = textConnection(model_code))



## ------------------------------------------------------------------------
print(model_run_2)


## ---- eval = FALSE-------------------------------------------------------
## model_code ='
## model {
##   # Likelihood
##   for(i in 1:N) {
##     y[i] ~ dnorm(intercept + slope*x[i], residual_sd^-2)
##   }
##   # Priors
##   intercept ~ dnorm(9,0.5^-2)
##   slope ~ dnorm(0,100^-2)
##   residual_sd ~ dunif(0,100)
## }
## '


## ---- eval = FALSE-------------------------------------------------------
## stan_code = '
## data {
##   int N;
##   vector[N] x;
##   vector[N] y;
## }
## parameters {
##   real intercept;
##   real slope;
##   real<lower=0> residual_sd;
## }
## model {
##   // Likelihood
##   y ~ normal(intercept + slope * x, residual_sd);
##   // Priors
##   intercept ~ normal(0, 100);
##   slope ~ normal(0, 100);
##   residual_sd ~ uniform(0, 100);
## }
## '


## ---- eval = FALSE-------------------------------------------------------
## stan_run = stan(data = list(N = nrow(dat),
##                             y = log(dat$earn),
##                             x = dat$height_cm),
##                 model_code = stan_code)
## print(stan_run)
## plot(stan_run)


## ------------------------------------------------------------------------
jags_code = '
model{
  # Likelihood
  for(i in 1:N) {
    y[i] ~ dnorm(intercept + slope * x[i],
                    residual_sd^-2)
  }
  # Priors
  intercept ~ dnorm(0, 100^-2)
  slope ~ dnorm(0, 100^-2)
  residual_sd ~ dunif(0, 100)
}
'


## ---- include = FALSE----------------------------------------------------
library(R2jags)
#library(rjags)


## ---- results = 'hide', message=FALSE------------------------------------
jags_run = jags(data = list(N = nrow(dat),
                            y = log(dat$earn),
                            x = dat$height_cm),
                parameters.to.save = c('intercept',
                                       'slope',
                                       'residual_sd'),
                model.file = textConnection(jags_code))
print(jags_run)
plot(jags_run)


## ----echo=FALSE----------------------------------------------------------
posterior = model_run$BUGSoutput$sims.matrix
head(posterior, 3)

# Class 4 -----------------------------------------------------------------

## ------------------------------------------------------------------------
N = 10
x = 1:N
y = rnorm(N, mean = -2 + 0.4 * x, sd = 1)


## ------------------------------------------------------------------------
eps = rnorm(N, mean = 0, sd = 1)
y = -2 + 0.4 * x + eps


## ---- eval=FALSE---------------------------------------------------------
## y = rnorm(N, mean = -2 + 0.4 * x1 - 0.3 * x2, sd = 1)
## p = inv.logit(-2 + 0.4 * x1 - 0.3 * x2)
## y = rbinom(N, size = 1, prob = p)


## ---- eval=FALSE---------------------------------------------------------
## y = rnorm(N, mean = -2 + 0.4 * x1 - 0.3 * x2 +
##             0.05 * x1 * x2, sd = 1)


## ---- eval=FALSE---------------------------------------------------------
## y = rnorm(N, mean = -2 + 0.4 * x1 - 0.3 * x1^2 +
##             0.05 * x1 * x2, sd = 1)


## ---- message=FALSE, results='hide'--------------------------------------
library(R2jags)
dat = read.csv('data/earnings.csv') # Called dat
jags_code = '
model{
  # Likelihood
  for(i in 1:N) {
    y[i] ~ dnorm(alpha + beta1*x1[i] + beta2*x2[i], sigma^-2)
  }
  # Priors
  alpha ~ dnorm(0, 20^-2)
  beta1 ~ dnorm(0, 1^-2)
  beta2 ~ dnorm(0, 10^-2)
  sigma ~ dunif(0, 10)
}
'
jags_run = jags(data = list(N = nrow(dat),
                            y = log(dat$earn),
                            x1 = dat$height_cm,
                            x2 = as.integer(dat$eth ==3)),
                parameters.to.save = c('alpha',
                                       'beta1',
                                       'beta2',
                                       'sigma'),
                model.file = textConnection(jags_code))


## ------------------------------------------------------------------------
print(jags_run)


## ------------------------------------------------------------------------
post = jags_run$BUGSoutput$sims.matrix
head(post)


## ---- eval=FALSE---------------------------------------------------------
## alpha_mean = mean(post[,'alpha'])
## beta1_mean = mean(post[,'beta1'])
## beta2_mean = mean(post[,'beta2'])
## plot(dat$height_cm, log(dat$earn))
## lines(dat$height_cm, alpha_mean +
##         beta1_mean * dat$height_cm)
## lines(dat$height_cm, alpha_mean +
##         beta1_mean * dat$height_cm, col = 'red')
## lines(dat$height_cm, alpha_mean +
##         beta1_mean * dat$height_cm + beta2_mean,
##       col = 'blue')


## ---- echo = FALSE-------------------------------------------------------
alpha_mean = mean(post[,'alpha'])
beta1_mean = mean(post[,'beta1'])
beta2_mean = mean(post[,'beta2'])
plot(dat$height_cm, log(dat$earn))
lines(dat$height_cm, alpha_mean +
        beta1_mean * dat$height_cm, col = 'red')
lines(dat$height_cm, alpha_mean +
        beta1_mean * dat$height_cm + beta2_mean,
      col = 'blue')


## ------------------------------------------------------------------------
stan_code = '
data {
  int<lower=0> N;
  vector[N] y;
  vector[N] x1;
  vector[N] x2;
}
parameters {
  real alpha;
  real beta1;
  real beta2;
  real<lower=0> sigma;
}
model {
  y ~ normal(alpha + x1 * beta1  + x2 * beta2, sigma);
}
'


## ---- fig.height = 5, message=FALSE, results='hide'----------------------
library(rstan)
stan_run = stan(data = list(N = nrow(dat),
                            y = log(dat$earn),
                            x1 = dat$height_cm,
                            x2 = as.integer(dat$eth==3)),
                model_code = stan_code)


## ---- message=FALSE------------------------------------------------------
plot(stan_run)


## ---- warning=FALSE------------------------------------------------------
y = rbinom(N, size = 1, prob = -2 + 0.4 * x)


## ------------------------------------------------------------------------
-2 + 0.4 * x
exp(-2 + 0.4 * x)/(1 + exp(-2 + 0.4 * x))


## ------------------------------------------------------------------------
library(boot)
p = inv.logit(-2 + 0.4 * x)
y = rbinom(N, size = 1, prob = p)
y


## ------------------------------------------------------------------------
lambda = exp(-2 + 0.4 * x)
y = rpois(N, lambda)
y


## ------------------------------------------------------------------------
swt = read.csv('data/swt.csv')
head(swt)


## ---- message = FALSE, results = 'hide'----------------------------------
jags_code = '
model{
  # Likelihood
  for(i in 1:N) {
    y[i] ~ dbin(p[i], 1)
    logit(p[i]) <- alpha + beta*x[i]
  }
  # Priors
  alpha ~ dnorm(0, 20^-2)
  beta ~ dnorm(0, 20^-2)
}
'
jags_run = jags(data = list(N = nrow(swt),
                            y = swt$rep.1,
                            x = swt$forest),
                parameters.to.save = c('alpha',
                                       'beta'),
                model.file = textConnection(jags_code))


## ---- echo = FALSE-------------------------------------------------------
pars = jags_run$BUGSoutput$sims.matrix
par(mfrow=c(1,2))
hist(pars[,'alpha'], breaks = 30)
hist(pars[,'beta'], breaks = 30)
par(mfrow=c(1,1))


## ---- fig.height = 4-----------------------------------------------------
plot(swt$forest, swt$rep.1)
points(swt$forest,
       inv.logit(mean(pars[,'alpha']) +
                   mean(pars[,'beta'])*swt$forest ),
       col = 'red')


## ------------------------------------------------------------------------
jags_code = '
model{
  # Likelihood
  for(i in 1:N) {
    y[i] ~ dpois(lambda[i])
    log(lambda[i]) <- alpha + beta*x[i]
  }
  # Priors
  alpha ~ dnorm(0, 20^-2)
  beta ~ dnorm(0, 20^-2)
}
'


## ---- include = FALSE----------------------------------------------------
# Adapted data from Royle and Dorazio (Chapter 2)
n = 12 # Number of experiments
total_moths = 20 # Number of moths in each experiment
num_died = c(1,4,9,13,18,20, 0,2,6,10,12,16) # Number died
sex = c(rep('male',6), rep('female',6)) # Sex
dose = rep(0:5, 2) # Dose of ???
sex_male = as.integer(sex == 'male') # Turn sex into an integer
# The key questions is: what are the effects of dose and sex?

## ---- fig.height = 5-----------------------------------------------------
plot(dose, num_died, col = sex_male + 1)
legend('topleft', legend = c('Female', 'Male'),
       col = 1:2, pch = 1)


## ---- fig.height = 5-----------------------------------------------------
plot(dose, num_died/total_moths, col = sex_male + 1, las = 1)
legend('topleft', legend = c('Female', 'Male'),
       col = 1:2, pch = 1)


## ------------------------------------------------------------------------
model_code = '
model
{
  # Likelihood
  for (i in 1:N) {
    y[i] ~ dbin(p_died[i], K)
    logit(p_died[i]) <- alpha + beta_male * x_male[i] + beta_dose * x_dose[i]
  }
  # Priors
  alpha ~ dnorm(0, 10^-2)
  beta_male ~ dnorm(0, 10^-2)
  beta_dose ~ dnorm(0, 10^-2)
}
'
model_data = list(N = N, y = num_died, x_male = sex_male, x_dose = dose, K = total_moths)
model_parameters =  c("alpha", "beta_male", "beta_dose")
model_run = jags(data = model_data,
                 parameters.to.save = model_parameters,
                 model.file = textConnection(model_code))


## ------------------------------------------------------------------------
plot(model_run)


## ---- fig.width = 8, fig.height = 3.5------------------------------------
plot(post[,'alpha'], type = 'l')


## ------------------------------------------------------------------------
apply(post,2, quantile, probs = c(0.025, 0.975))


## ---- fig.height = 3-----------------------------------------------------
hist(post[,'beta2'], breaks = 30)


## ------------------------------------------------------------------------
jags_code = '
model{
  # Likelihood
  for(i in 1:N) {
    y[i] ~ dnorm(alpha + beta1*x1[i] + beta2*x2[i],
                  sigma^-2)
    y_sim[i] ~ dnorm(alpha + beta1*x1[i] + beta2*x2[i],
                      sigma^-2)
  }
  # Priors
  alpha ~ dnorm(0, 20^-2)
  beta1 ~ dnorm(0, 1^-2)
  beta2 ~ dnorm(0, 10^-2)
  sigma ~ dunif(0, 10)
}
'

## ---- include = FALSE, results = 'hide', message=FALSE-------------------
jags_run = jags(data = list(N = nrow(dat),
                            y = log(dat$earn),
                            x1 = dat$height_cm,
                            x2 = as.integer(dat$eth==3)),
                parameters.to.save = c('y_sim'),
                model.file = textConnection(jags_code))
pars = jags_run$BUGSoutput$sims.list$y_sim

## ---- echo = FALSE-------------------------------------------------------
plot(log(dat$earn), apply(pars,2,'mean'))
abline(a=0, b = 1, col = 'red')

