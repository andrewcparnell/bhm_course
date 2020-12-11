# Taken from classes 5 and 6

# Class 5

## ----setup, include=FALSE------------------------------------------------
N = 100
x = sort(runif(N, 150, 190))


## ---- fig.height = 4-----------------------------------------------------
dat = read.csv('../data/earnings.csv') # Might need to set wd before running this
alpha = rnorm(1, mean = 10, sd = 2)
beta = rnorm(1, mean = 0, sd = 0.1)
sigma = runif(1, 0, 5)
y = rnorm(N, alpha + beta * (x - mean(x)), sd = sigma)
plot(x, y)
lines(x, alpha + beta * (x - mean(x)), col = 'red')


## ---- message=FALSE, results='hide'--------------------------------------
library(R2jags)
jags_code = '
model{
  # Likelihood
  for(i in 1:N) {
    y[i] ~ dnorm(alpha + beta*(x[i] - mean(x)),
                  sigma^-2)
    y_sim[i] ~ dnorm(alpha + beta*(x[i] - mean(x)),
                      sigma^-2)
  }
  # Priors
  alpha ~ dnorm(11, 2^-2)
  beta ~ dnorm(0, 0.1^-2)
  sigma ~ dunif(0, 5)
}
'

## ---- messages=FALSE, include = FALSE------------------------------------
jags_run = jags(data = list(N = nrow(dat),
                            y = log(dat$earn),
                            x = dat$height_cm),
                parameters.to.save = c('alpha',
                                       'beta',
                                       'sigma',
                                       'y_sim'),
                model.file = textConnection(jags_code))


## ---- echo = FALSE-------------------------------------------------------
pars = jags_run$BUGSoutput$sims.list
par(mfrow=c(1,3))
dens = density(pars$alpha)
curve(dnorm(x, mean = 11, sd = 2), 6, 16, xlab = 'alpha', ylab = '', ylim = range(dens$y))
lines(dens, col='red')
dens = density(pars$beta)
curve(dnorm(x, mean = 0, sd = 0.1), -0.5, 0.5, xlab = 'beta', ylab = '', ylim = range(dens$y))
lines(dens, col='red')
dens = density(pars$sigma)
curve(dunif(x, 0, 5), 0, 5, xlab = 'sigma', ylab = '', ylim = range(dens$y))
lines(dens, col='red')
par(mfrow=c(1,1))


## ---- fig.height=4-------------------------------------------------------
pred = pars$y_sim
y_sim_summary = apply(pred, 2, 'quantile',
                      probs = c(0.05, 0.5, 0.95))
plot(log(dat$earn), y_sim_summary[2,],
     xlab = 'True y value',
     ylab = 'Posterior Predicted y value')
abline(a=0, b=1, col = 'red')


## ------------------------------------------------------------------------
jags_code = '
model{
  # Likelihood
  for(i in 1:N) {
    y[i] ~ dnorm(alpha[eth[i]] +
                  beta[eth[i]]*(x[i] - mean(x)),
                    sigma^-2)
  }
  # Priors
  for(j in 1:N_eth) {
    alpha[j] ~ dnorm(mu_alpha, sigma_alpha^-2)
    beta[j] ~ dnorm(mu_beta, sigma_beta^-2)
  }
  mu_alpha ~ dnorm(11, 2^-2)
  mu_beta ~ dnorm(0, 0.1^-2)
  sigma ~ dunif(0, 5)
  sigma_alpha ~ dunif(0, 2)
  sigma_beta ~ dunif(0, 2)
}
'


## ---- message=FALSE, results='hide'--------------------------------------
jags_run = jags(data = list(N = nrow(dat),
                            y = log(dat$earn),
                            eth = dat$eth,
                            N_eth = length(unique(dat$eth)),
                            x = dat$height_cm),
                parameters.to.save = c('alpha',
                                       'beta',
                                       'sigma',
                                       'mu_alpha',
                                       'mu_beta',
                                       'sigma_alpha',
                                       'sigma_beta'),
                model.file = textConnection(jags_code))


## ---- echo = FALSE-------------------------------------------------------
pars = jags_run$BUGSoutput$mean
par(mfrow=c(2,2))
eth_names = c('Blacks','Hispanics','Whites','Others')
for(i in 1:4) {
  curr_dat = subset(dat, dat$eth == i)
  plot(curr_dat$height_cm, log(curr_dat$earn), main = eth_names[i], ylab = 'log(earnings)', xlab = 'Height (cm)')
  lines(dat$height_cm, pars$alpha[i] + pars$beta[i]*(dat$height_cm - mean (dat$height_cm)), col = i)
}
par(mfrow=c(1,1))


## ---- echo = FALSE-------------------------------------------------------
par(mfrow=c(2,2))
pars = jags_run$BUGSoutput$sims.list
for(i in 1:4) {
  hist(pars$beta[,i], breaks = 30, main = eth_names[i], xlim = range(pars$beta), xlab = 'Slope value')
}
par(mfrow=c(1,1))


## ------------------------------------------------------------------------
jags_code = '
model{
  # Likelihood
  for(i in 1:N) {
    y[i] ~ dnorm(alpha[eth[i]] +
                  beta[eth[i]]*(x[i] - mean(x)),
                    sigma^-2)
  }
  # Priors
  for(j in 1:N_eth) {
    alpha[j] ~ dnorm(11, 10^-2)
    beta[j] ~ dnorm(0, 0.1^-2)
  }
  sigma ~ dunif(0, 5)
}
'


## ---- echo = FALSE, message=FALSE, results = 'hide'----------------------
jags_run = jags(data = list(N = nrow(dat),
                            y = log(dat$earn),
                            eth = dat$eth,
                            N_eth = length(unique(dat$eth)),
                            x = dat$height_cm),
                parameters.to.save = c('alpha',
                                       'beta',
                                       'sigma'),
                model.file = textConnection(jags_code))
eth_names = c('Blacks','Hispanics','Whites','Others')

## ---- echo = FALSE-------------------------------------------------------
par(mfrow=c(2,2))
pars = jags_run$BUGSoutput$sims.list
for(i in 1:4) {
  hist(pars$beta[,i], breaks = 30, main = eth_names[i], xlim = range(pars$beta), xlab = 'Slope value')
}
par(mfrow=c(1,1))


## ------------------------------------------------------------------------
jags_code = '
model{
  # Likelihood
  for(i in 1:N) {
    y[i] ~ dnorm(alpha +
                  beta*(x[i] - mean(x)),
                    sigma^-2)
  }
  for(j in 1:N_pred) {
    y_pred[j] ~ dnorm(alpha +
                        beta*(x_pred[j] - mean(x)),
                          sigma^-2)
  }
  alpha ~ dnorm(11, 2^-2)
  beta ~ dnorm(0, 0.1^-2)
  sigma ~ dunif(0, 5)
}
'


## ---- eval=FALSE---------------------------------------------------------
## folds = sample(rep(1:5, length = nrow(dat)))
## rmsep = rep(NA, nrow(dat))
## for(i in 1:5) {
##   curr_data = subset(dat, folds != i)
##   curr_pred = subset(dat, folds == i)
##   jags_run = jags(data = list(N = nrow(curr_data),
##                             y = log(curr_data$earn),
##                             x = curr_data$height_cm,
##                             N_pred = nrow(curr_pred),
##                             x_pred = curr_pred$height_cm),
##                 parameters.to.save = 'y_pred',
##                 model.file = textConnection(jags_code))
##   out_pred = jags_run$BUGSoutput$mean$y_pred
##   rmsep[folds ==i] = (log(curr_pred$earn) - out_pred)^2
## }
## sqrt(mean(rmsep))

# Class 6 -----------------------------------------------------------------

## ------------------------------------------------------------------------
swt = read.csv('../data/swt.csv')
head(swt)


## ---- message = FALSE, results = 'hide'----------------------------------
jags_code = '
model{
  # Likelihood
  for(i in 1:N) {
    y[i] ~ dbin(p[i], N_exp[i])
    logit(p[i]) <- alpha[alt[i]] + beta[alt[i]]* (x[i] - mean(x))
  }
  # Priors
  for(j in 1:N_alt) {
    alpha[j] ~ dnorm(mu_alpha, sigma_alpha^-2)
    beta[j] ~ dnorm(mu_beta, sigma_beta^-2)
  }
  mu_alpha ~ dnorm(0, 5^-2)
  mu_beta ~ dnorm(0, 0.1^-2)
  sigma_alpha ~ dt(0,5^-2,1)T(0,)
  sigma_beta ~ dt(0,5^-2,1)T(0,)
}
'


## ---- echo = FALSE, message=FALSE, results = 'hide'----------------------
sum_fun = function(x) {
  s = ifelse(is.na(x[1]),0,x[1]) + ifelse(is.na(x[2]),0,x[2]) + ifelse(is.na(x[3]),0,x[3])
  N = ifelse(is.na(x[1]),0,1) + ifelse(is.na(x[2]),0,1) + ifelse(is.na(x[3]),0,1)
  return(c(s,N))
}
y = apply(swt[,1:3],1,sum_fun)[1,]
N = apply(swt[,1:3],1,sum_fun)[2,]
library(R2jags)
jags_run = jags(data = list(N = nrow(swt),
                            N_exp = N,
                            N_alt = length(unique(swt$alt)),
                            alt = swt$alt,
                            y = y,
                            x = swt$forest),
                parameters.to.save = c('alpha',
                                       'beta',
                                       'mu_alpha',
                                       'mu_beta',
                                       'sigma_alpha',
                                       'sigma_beta'),
                model.file = textConnection(jags_code))

## ---- echo = FALSE-------------------------------------------------------
par(mfrow=c(1,3))
pars = jags_run$BUGSoutput$sims.list
for(i in c(2,3,1)) {
  hist(pars$alpha[,i], breaks = 30, main = paste('Altitude type:',levels(swt$alt)[i]), xlim = range(pars$alpha), xlab = 'Intercept value')
}
par(mfrow=c(1,1))


## ---- echo = FALSE-------------------------------------------------------
par(mfrow=c(1,3))
pars = jags_run$BUGSoutput$sims.list
for(i in c(2,3,1)) {
  hist(pars$beta[,i], breaks = 30,
       main = paste('Altitude type:',levels(swt$alt)[i]),
       xlim = range(pars$beta), xlab = 'Slope value')
}
par(mfrow=c(1,1))


## ---- echo=FALSE---------------------------------------------------------
library(boot)
par(mfrow=c(1,3))
pars = jags_run$BUGSoutput$mean
for(i in c(2,3,1)) {
  plot(swt$forest, y/N, main = paste('Altitude type:',levels(swt$alt)[i]), xlab = '% forest cover', ylab = 'Estimated proporton')
  points(swt$forest, inv.logit(pars$alpha[i] + pars$beta[i] * swt$forest), col = i+1)
}
par(mfrow=c(1,1))


## ---- message = FALSE, results = 'hide'----------------------------------
jags_code = '
model{
  # Likelihood
  for(i in 1:N) {
    y[i] ~ dpois(exp(log_lambda[i]))
    log_lambda[i] ~ dnorm(alpha + beta * (x[i] - mean(x)),
        sigma^-2)
  }
  alpha ~ dnorm(0, 5^-2)
  beta ~ dnorm(0, 0.1^-2)
  sigma ~ dt(0,5^-2,1)T(0,)
}
'


## ---- echo = FALSE, message=FALSE, results = 'hide'----------------------
set.seed(123)
jags_run = jags(data = list(N = nrow(swt),
                            y = y,
                            x = swt$forest),
                parameters.to.save = c('alpha',
                                       'beta',
                                       'sigma'),
                n.iter = 5000,
                n.thin = 5,
                model.file = textConnection(jags_code))

## ---- echo = FALSE-------------------------------------------------------
par(mfrow=c(1,3))
pars = jags_run$BUGSoutput$sims.list
for(i in c(1:2,4)) {
  hist(pars[[i]], breaks = 30, main = names(pars)[i], xlab = 'Parameter value')
}
par(mfrow=c(1,1))


## ------------------------------------------------------------------------
curve(dnorm, from = -5, to = 5)
curve(dt(x, df = 1), add = TRUE, col = 'red')
curve(dt(x, df = 4), add = TRUE, col = 'blue')


## ---- echo = FALSE, fig.height= 6----------------------------------------
N = 100
set.seed(123)
x = runif(N)
y = rt(N, df = 3)*0.8 + 2 - 2*x
plot(x, y)


## ------------------------------------------------------------------------
jags_code = '
model{
  # Likelihood
  for(i in 1:N) {
    y[i] ~ dt(alpha + beta * (x[i] - mean(x)),
                sigma^-2, df[i] + 1)
    df[i] ~ dbin(p, 10)
  }
  p ~ dunif(0, 1)
  alpha ~ dnorm(0, 1^-2)
  beta ~ dnorm(0, 1^-2)
  sigma ~ dt(0,1,1)T(0,)
}
'


## ---- echo = FALSE, message=FALSE, results = 'hide'----------------------
jags_run = jags(data = list(N = N,
                            y = y,
                            x = x),
                parameters.to.save = c('alpha',
                                       'beta',
                                       'df'),
                model.file = textConnection(jags_code))


## ---- fig.height=6-------------------------------------------------------
dfs = jags_run$BUGSoutput$median$df
pars = jags_run$BUGSoutput$mean
plot(x, y, col = as.factor(dfs))
lines(x, as.numeric(pars$alpha) +
        as.numeric(pars$beta)*(x - mean(x)),
      col = 'red')


## ------------------------------------------------------------------------
jags_code = '
model{
  # Likelihood
  for(i in 1:N) {
    z[i] ~ dnorm(alpha + beta * (x[i] - mean(x)),
                    sigma^-2)
    y[i] ~ dinterval(z[i], cuts)
  }
  alpha ~ dnorm(0, 100^-2)
  beta ~ dnorm(0, 100^-2)
  sigma ~ dt(0, 10^-2, 1)T(0, )
}
'


## ------------------------------------------------------------------------
N = 100
alpha = -1
beta = 0.2
sigma = 0.51
set.seed(123)
x = runif(N, 0, 10)
cuts = c(-0.5, 0.5)
z = rnorm(N, alpha + beta * (x - mean(x)), sigma)
y = findInterval(z, cuts)


## ------------------------------------------------------------------------
plot(x, z, col = y + 1)


## ---- message=FALSE, results = 'hide'------------------------------------
jags_inits = function() {
  z = runif(N, -0.5, 0.5)
  z[y==0] = runif(sum(y==0), -1, -0.5)
  z[y==2] = runif(sum(y==2), 0.5, 1)
  return(list(z = z))
}
jags_run = jags(data = list(N = N,
                            y = y,
                            x = x,
                            cuts = cuts),
                inits = jags_inits,
                parameters.to.save = c('alpha',
                                       'beta',
                                       'sigma'),
                model.file = textConnection(jags_code))


## ------------------------------------------------------------------------
print(jags_run)

