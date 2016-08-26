# Multivariate model with two response variables

# y[i,1] = response variable 1 for observation i
# y[i,2] = response variable 2 for ob i

# y[i,] ~ MVN(mu[i,], Sigma)
# mu[i,1] <- alpha_1 + beta_1 * x[i]
# mu[i,2] <- alpha_1 + beta_1 * x[i]

jags_code = '
model{
# Likelihood
for(i in 1:N) {
  y_1[i] ~ dpois(exp(log_lambda[i,1]))
  y_2[i] ~ dpois(exp(log_lambda[i,2]))
  log_lambda[i,1:2] ~ dmnorm(mu_lambda[i,1:2], Sigma_lambda_inv)
  mu_lambda[i,1] = alpha_1 + beta_1 * flower_abund[i]))
  mu_lambda[i,2] = alpha_2 + beta_2 * flower_abund[i]))
}
Sigma_lambda = inverse(Sigma_lambda_inv)
rho <- Sigma_lambda[1,2] / sqrt( Sigma_lambda[1,1] * Sigma_lambda[2,2])
Sigma_beta_inv ~ dwish(R_beta,k_beta)

# Priors
# Need priors for everything here
}
'