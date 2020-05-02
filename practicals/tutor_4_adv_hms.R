# Taken from classes 7 and 8

set.seed(123)
par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=-.01,las=1)
#setwd("slides")

# First class 7

jags_code = '
model {
  # Likelihood
  for(i in 1:N) {
    y[i] ~ dnorm(alpha[eth[i]] + beta[eth[i]]*(x[i] - mean(x)),
      sigma^-2)
  }
  # Priors
  for(j in 1:N_eth) {
    alpha[j] ~ dnorm(mu_alpha, sigma_alpha^-2)
    beta[j] ~ dnorm(mu_beta, sigma_beta^-2)
  }
  mu_alpha ~ dnorm(mu_alpha_0, sigma_alpha_0^-2)
  mu_alpha_0 ~ dnorm(0, 20^-2)
  sigma_alpha_0 ~ dt(0,5^-2,1)T(0,)
  mu_beta ~ dnorm(mu_beta_0, sigma_beta_0^-2)
  mu_beta_0 ~ dnorm(0, 20^-2)
  sigma_beta_0 ~ dt(0,5^-2,1)T(0,)
  sigma ~ dunif(0, 5)
  sigma_alpha ~ dt(0,5^-2,1)T(0,)
  sigma_beta ~ dt(0,5^-2,1)T(0,)
}
'

## ---- message=FALSE, results='hide', echo = FALSE------------------------
library(R2jags)
dat = read.csv('../data/earnings.csv')
jags_run = jags(data = list(N = nrow(dat),
                            y = log(dat$earn),
                            eth = dat$eth,
                            N_eth = length(unique(dat$eth)),
                            x = dat$height_cm),
                parameters.to.save = c('mu_alpha_0',
                                       'sigma_alpha_0',
                                       'mu_beta_0',
                                       'sigma_beta_0'),
                model.file = textConnection(jags_code))

## ---- echo = FALSE-------------------------------------------------------
pars = jags_run$BUGSoutput$sims.matrix
par(mfrow=c(2,2))
for(i in 1:4) {
  hist(pars[,i+1], main= colnames(pars)[i+1], xlab = '', breaks = 30)
}
par(mfrow=c(1,1))

## ---- echo = FALSE, fig.height=4-----------------------------------------
dat = read.csv('../data/earnings.csv')
par(mfrow=c(1,2))
plot(jitter(dat$height_cm), dat$earn, xlab = 'Height (cm)', ylab = 'Earnings ($)')
plot(jitter(dat$height_cm), log(dat$earn), xlab = 'Height (cm)', ylab = 'log(Earnings ($))')
par(mfrow=c(1,1))

## ---- include = FALSE----------------------------------------------------
jags_code = '
model {
  # Likelihood
  for(i in 1:N) {
  log_earn[i] ~ dnorm(alpha +
    beta_height*(height[i] - mean(height)), sigma^-2)
  }
  # Priors
  alpha ~ dnorm(0, 20^-2)
  beta_height ~ dnorm(0, 20^-2)
  sigma ~ dt(0,10^-2,1)T(0,)
}
'
jags_run = jags(data = list(N = nrow(dat),
                            log_earn = log(dat$earn),
                            height = dat$height_cm),
                parameters.to.save = c('alpha',
                                       'beta_height',
                                       'sigma'),
                model.file = textConnection(jags_code))

## ---- echo = FALSE-------------------------------------------------------
print(jags_run)

## ---- include = FALSE----------------------------------------------------
jags_summ = jags_run$BUGSoutput$summary
low = signif(exp(jags_summ['alpha','2.5%']),2)
high = signif(exp(jags_summ['alpha','97.5%']),2)
sig = jags_summ['sigma','mean']
beta = jags_summ['beta_height','mean']
DIC = round(jags_run$BUGSoutput$DIC,2)
pD = round(jags_run$BUGSoutput$pD,2)

## ------------------------------------------------------------------------
jags_code = '
model {
  # Likelihood
  for(i in 1:N) {
    log_earn[i] ~ dnorm(alpha_eth[eth[i]] +
      beta_height*(height[i] - mean(height)),
        sigma^-2)
  }
  # Priors
  for(j in 1:N_eth) {
    alpha_eth[j] ~ dnorm(mu_eth, sigma_eth^-2)
  }
  beta_height ~ dnorm(0, 20^-2)
  mu_eth ~ dnorm(0, 20^-2)
  sigma_eth ~ dt(0,10^-2,1)T(0,)
  sigma ~ dt(0,10^-2,1)T(0,)
}
'

## ------------------------------------------------------------------------
jags_run = jags(data = list(N = nrow(dat),
                            N_eth = length(unique(dat$eth)),
                            log_earn = log(dat$earn),
                            height = dat$height_cm,
                            eth = dat$eth),
                parameters.to.save = c('alpha_eth',
                                       'beta_height',
                                       'mu_eth',
                                       'sigma'),
                model.file = textConnection(jags_code))

## ---- echo = FALSE-------------------------------------------------------
print(jags_run)

## ---- include = FALSE----------------------------------------------------
jags_summ = jags_run$BUGSoutput$summary
low = signif(exp(jags_summ['mu_eth','2.5%']),2)
high = signif(exp(jags_summ['mu_eth','97.5%']),2)
DIC = round(jags_run$BUGSoutput$DIC,2)
pD = round(jags_run$BUGSoutput$pD,2)

## ------------------------------------------------------------------------
jags_code = '
model {
  # Likelihood
  for(i in 1:N) {
    log_earn[i] ~ dnorm(alpha_eth[eth[i]] +
      beta_height[eth[i]]*(height[i] - mean(height)),
        sigma^-2)
  }
  # Priors
  for(j in 1:N_eth) {
    alpha_eth[j] ~ dnorm(mu_eth, sigma_eth^-2)
    beta_height[j] ~ dnorm(mu_beta_height, sigma_height^-2)
  }
  mu_beta_height ~ dnorm(0, 20^-2)
  mu_eth ~ dnorm(0, 20^-2)
  sigma_eth ~ dt(0,10^-2,1)T(0,)
  sigma_height ~ dt(0,10^-2,1)T(0,)
  sigma ~ dt(0,10^-2,1)T(0,)
}
'

## ---- include = FALSE----------------------------------------------------
jags_run = jags(data = list(N = nrow(dat),
                            N_eth = length(unique(dat$eth)),
                            log_earn = log(dat$earn),
                            height = dat$height_cm,
                            eth = dat$eth),
                parameters.to.save = c('alpha_eth',
                                       'beta_height',
                                       'beta_eth',
                                       'mu_eth',
                                       'mu_beta_height',
                                       'sigma_height',
                                       'sigma'),
                model.file = textConnection(jags_code))

## ---- echo = FALSE-------------------------------------------------------
print(jags_run)

## ---- include = FALSE----------------------------------------------------
jags_summ = jags_run$BUGSoutput$summary
low = signif(exp(jags_summ['mu_eth','2.5%']),2)
high = signif(exp(jags_summ['mu_eth','97.5%']),2)
DIC = round(jags_run$BUGSoutput$DIC,2)
pD = round(jags_run$BUGSoutput$pD,2)

## ---- include = FALSE----------------------------------------------------
jags_code = '
model {
  # Likelihood
  for(i in 1:N) {
    log_earn[i] ~ dnorm(alpha_eth[eth[i]] +
      beta_height[eth[i]]*(height[i] - mean(height)),
        sigma^-2)
    log_earn_pred[i] ~ dnorm(alpha_eth[eth[i]] +
      beta_height[eth[i]]*(height[i] - mean(height)),
        sigma^-2)
  }
  # Priors
  for(j in 1:N_eth) {
    alpha_eth[j] ~ dnorm(mu_eth, sigma_eth^-2)
    beta_height[j] ~ dnorm(mu_beta_height, sigma_height^-2)
  }
  mu_beta_height ~ dnorm(0, 20^-2)
  mu_eth ~ dnorm(0, 20^-2)
  sigma_eth ~ dt(0,10^-2,1)T(0,)
  sigma_height ~ dt(0,10^-2,1)T(0,)
  sigma ~ dt(0,10^-2,1)T(0,)
}
'
jags_run = jags(data = list(N = nrow(dat),
                            N_eth = length(unique(dat$eth)),
                            log_earn = log(dat$earn),
                            height = dat$height_cm,
                            eth = dat$eth),
                parameters.to.save = c('log_earn_pred'),
                model.file = textConnection(jags_code))

## ---- echo = FALSE-------------------------------------------------------
plot(log(dat$earn), jags_run$BUGSoutput$mean$log_earn_pred, xlab = 'True log(earnings)', ylab = 'Predicted log(earnings)')
abline(a = 0, b = 1, col = 'red')

## ------------------------------------------------------------------------
jags_code = '
model {
  # Likelihood
  for(i in 1:N) {
    log_earn[i] ~ dnorm(alpha[eth[i],age_grp[i]] +
      beta[eth[i],age_grp[i]]*(height[i] - mean(height)),
        sigma^-2)
  }
  # Priors
  for(j in 1:N_eth) {
    for(k in 1:N_age_grp) {
      alpha[j,k] ~ dnorm(mu_alpha, sigma_alpha^-2)
      beta[j,k] ~ dnorm(mu_beta, sigma_beta^-2)
    }
  }
  mu_alpha ~ dnorm(0, 20^-2)
  mu_beta ~ dnorm(0, 20^-2)
  sigma_alpha ~ dt(0,10^-2,1)T(0,)
  sigma_beta ~ dt(0,10^-2,1)T(0,)
  sigma ~ dt(0,10^-2,1)T(0,)
}
'

## ---- include = FALSE----------------------------------------------------
jags_run = jags(data = list(N = nrow(dat),
                            N_eth = length(unique(dat$eth)),
                            N_age_grp = length(unique(dat$age)),
                            log_earn = log(dat$earn),
                            height = dat$height_cm,
                            eth = dat$eth,
                            age_grp = dat$age),
                parameters.to.save = c('alpha',
                                       'beta'),
                model.file = textConnection(jags_code))

## ---- echo = FALSE-------------------------------------------------------
pars = jags_run$BUGSoutput$mean
age_grp_names = c('18-34','35-49','50-64')
eth_names = c('Blacks','Hispanics','Whites','Others')

par(mfrow=c(4,3))
for(i in 1:4) {
  for(j in 1:3) {
    curr_dat = subset(dat, dat$eth == i & dat$age == j)
    plot(curr_dat$height_cm, log(curr_dat$earn), main = paste(eth_names[i], age_grp_names[j]), ylab = 'log(earnings)', xlab = 'Height (cm)')
    lines(dat$height_cm, pars$alpha[i,j] + pars$beta[i,j]*(dat$height_cm - mean (dat$height_cm)), col = i)
  }
}
par(mfrow=c(1,1))

## ---- include = FALSE----------------------------------------------------
jags_summ = jags_run$BUGSoutput$summary
DIC = round(jags_run$BUGSoutput$DIC,2)
pD = round(jags_run$BUGSoutput$pD,2)

## ------------------------------------------------------------------------
jags_code = '
model {
  # Likelihood
  for(i in 1:N) {
    log_earn[i] ~ dnorm(beta[eth[i],age_grp[i],1] +
      beta[eth[i],age_grp[i],2]*(height[i] - mean(height)),
        sigma^-2)
  }
  # Priors
  for(j in 1:N_eth) {
    for(k in 1:N_age_grp) {
      beta[j,k,1:2] ~ dmnorm(mu_beta[1:2,1], Sigma_beta_inv)
    }
  }
  for(l in 1:2) {
    mu_beta[l,1] ~ dnorm(0, 20^-2)
  }
  Sigma_beta_inv ~ dwish(R_beta,k_beta)
  sigma ~ dt(0,10^-2,1)T(0,)
}
'

## ---- include = FALSE----------------------------------------------------
jags_run = jags(data = list(N = nrow(dat),
                            N_eth = length(unique(dat$eth)),
                            N_age_grp = length(unique(dat$age)),
                            log_earn = log(dat$earn),
                            height = dat$height_cm,
                            eth = dat$eth,
                            age_grp = dat$age,
                            k_beta = 2,
                            R_beta = diag(2)),
                parameters.to.save = c('beta'),
                model.file = textConnection(jags_code))

## ------------------------------------------------------------------------
print(jags_run)

## ---- include = FALSE----------------------------------------------------
jags_summ = jags_run$BUGSoutput$summary
DIC = round(jags_run$BUGSoutput$DIC,2)
pD = round(jags_run$BUGSoutput$pD,2)

## ---- echo = FALSE-------------------------------------------------------
pars = jags_run$BUGSoutput$mean
age_grp_names = c('18-34','35-49','50-64')
eth_names = c('Blacks','Hispanics','Whites','Others')

par(mfrow=c(4,3))
for(i in 1:4) {
  for(j in 1:3) {
    curr_dat = subset(dat, dat$eth == i & dat$age == j)
    plot(curr_dat$height_cm, log(curr_dat$earn), main = paste(eth_names[i], age_grp_names[j]), ylab = 'log(earnings)', xlab = 'Height (cm)')
    lines(dat$height_cm, pars$beta[i,j,1] + pars$beta[i,j,2]*(dat$height_cm - mean (dat$height_cm)), col = i)
  }
}
par(mfrow=c(1,1))

## ------------------------------------------------------------------------
dat2 = dat
dat2$earn[c(177, 763, 771)] = NA

## ---- include = FALSE----------------------------------------------------
jags_run = jags(data = list(N = nrow(dat2),
                            N_eth = length(unique(dat2$eth)),
                            N_age_grp = length(unique(dat2$age)),
                            log_earn = log(dat2$earn),
                            height = dat2$height_cm,
                            eth = dat2$eth,
                            age_grp = dat2$age,
                            k_beta = 2,
                            R_beta = diag(2)),
                parameters.to.save = c('log_earn[177]','log_earn[763]','log_earn[771]'),
                model.file = textConnection(jags_code))

## ------------------------------------------------------------------------
print(jags_run)


# Class 8 -----------------------------------------------------------------

## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(dev = 'pdf')
#knitr::opts_knit$set(global.par = TRUE)
set.seed(123)
library(R2jags)

## ---- include=FALSE------------------------------------------------------
par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=-.01,las=1)

## ------------------------------------------------------------------------
wf = read.csv('../data/whitefly.csv')
head(wf)


## ------------------------------------------------------------------------
barplot(table(wf$imm),
        main = 'Number of immature whiteflies')


## ------------------------------------------------------------------------
model_code = '
model
{
  # Likelihood
  for (i in 1:N) {
    y[i] ~ dpois(lambda[i])
    log(lambda[i]) <- beta_trt[trt[i]]
  }
  # Priors
  for (j in 1:N_trt) {
    beta_trt[j] ~ dnorm(0, 100^-2)
  }
}
'


## ---- message=FALSE, results='hide'--------------------------------------
jags_run = jags(data = list(N = nrow(wf),
                            N_trt = length(unique(wf$trt)),
                            y = wf$imm,
                            trt = wf$trt),
                parameters.to.save = 'beta_trt',
                model.file = textConnection(model_code))


## ---- fig.height=5-------------------------------------------------------
plot(jags_run)


## ---- fig.height=4-------------------------------------------------------
beta_means = jags_run$BUGSoutput$mean$beta_trt
y_sim_mean = exp(beta_means[wf$trt])
y_sim = rpois(nrow(wf), y_sim_mean)
hist(wf$imm, breaks = seq(0,max(wf$imm)))
hist(y_sim, breaks = seq(0,max(wf$imm)),
     add = TRUE, col = 'gray')


## ------------------------------------------------------------------------
model_code = '
model
{
  # Likelihood
  for (i in 1:N) {
    y[i] ~ dpois(lambda[i] * z[i] + 0.0001)
    log(lambda[i]) <- beta_trt[trt[i]]
    z[i] ~ dbinom(q_0, 1)
  }
  # Priors
  for (j in 1:N_trt) {
    beta_trt[j] ~ dnorm(0, 100^-2)
  }
  q_0 ~ dunif(0, 1)
}
'


## ---- message=FALSE, results='hide'--------------------------------------
jags_run = jags(data = list(N = nrow(wf),
                            N_trt = length(unique(wf$trt)),
                            y = wf$imm,
                            trt = wf$trt),
                parameters.to.save = c('beta_trt','q_0'),
                model.file = textConnection(model_code))


## ---- fig.height=5-------------------------------------------------------
plot(jags_run)


## ------------------------------------------------------------------------
beta_means = jags_run$BUGSoutput$mean$beta_trt
q_0_mean = jags_run$BUGSoutput$mean$q_0[1]
y_sim_mean = exp(beta_means[wf$trt])
rZIP = function(mean, q_0) {
  pois = rpois(length(mean), mean)
  pois[runif(length(mean))<q_0] = 0
  return(pois)
}
y_sim = rZIP(y_sim_mean, q_0_mean)


## ---- fig.height=5-------------------------------------------------------
hist(wf$imm, breaks = seq(0,max(wf$imm)))
hist(y_sim, breaks = seq(0,max(wf$imm)),
     add = TRUE, col = rgb(0.75,0.75,0.75,0.4))


## ------------------------------------------------------------------------
model_code = '
model
{
  # Likelihood
  for (i in 1:N) {
    y[i] ~ dpois(lambda[i])T(1,)
    log(lambda[i]) <- beta_trt[trt[i]]
  }
  for(i in 1:N_0) {
    y_0[i] ~ dbin(q_0, 1)
  }
  # Priors
  for (j in 1:N_trt) {
    beta_trt[j] ~ dnorm(0, 100^-2)
  }
  q_0 ~ dunif(0, 1)
}
'


## ---- message=FALSE, results='hide'--------------------------------------
jags_run = jags(data = list(N = nrow(wf[wf$imm > 0,]),
                            N_trt = length(unique(wf$trt)),
                            y = wf$imm[wf$imm > 0],
                            y_0 = as.integer(wf$imm == 0),
                            N_0 = nrow(wf),
                            trt = wf$trt[wf$imm > 0]),
                parameters.to.save = c('beta_trt', 'q_0'),
                model.file = textConnection(model_code))


## ---- fig.height = 5-----------------------------------------------------
plot(jags_run)


## ------------------------------------------------------------------------
pollen = read.csv('../data/pollen.csv')
head(pollen)


## ---- fig.height = 5, echo = FALSE---------------------------------------
pollen$S = rowSums(pollen[,3:ncol(pollen)])
plot(pollen$GDD5, pollen$Betula/pollen$S, xlab = 'Length of growing season', ylab = 'Estimated proportion')


## ------------------------------------------------------------------------
model_code = '
model
{
  # Likelihood
  for (i in 1:N) { # Observaton loops
    y[i,] ~ dmulti(p[i,], S[i])
    for(j in 1:M) { # Category loop
      exp_z[i,j] <- exp(z[i,j])
      p[i,j] <- exp_z[i,j]/sum(exp_z[i,])
      z[i,j] <- beta[j,]%*%x[i,]
    }
  }
  # Prior
  for(j in 1:M) {
    for(k in 1:K) {
      beta[j,k] ~ dnorm(0, 0.1^-2)
    }
  }
}
'


## ------------------------------------------------------------------------
model_data = list(N = nrow(pollen[1:500,]),
                  y = pollen[1:500,3:9],
                  x = cbind(1, scale(cbind(pollen[1:500,1:2],
                                           pollen[1:500,1:2]^2))),
                  S = pollen[1:500,10],
                  K = 5, # Number of covars
                  M = 7) # Number of categories
# Run the model
model_run = jags(data = model_data,
                 parameters.to.save = c("p"),
                 model.file = textConnection(model_code))


## ------------------------------------------------------------------------
plot(model_run)


## ---- fig.height = 5, echo = FALSE---------------------------------------
p3_model = model_run$BUGSoutput$mean$p[,3]
p3_data = pollen$Betula/pollen$S
plot(p3_data[1:500], p3_model,
     xlab = 'True proportion of Betula',
     ylab = 'Estimated proportion of Betula')
abline(a=0, b=1)

