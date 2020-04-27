# Header ------------------------------------------------------------------

# Fitting a linear regression in JAGS and calculate the R-squared value
# Andrew Parnell

# In this code we generate some data from a simple linear regression model and fit is using jags. We then compute an R-squared values

# Some boiler plate code to clear the workspace, and load in required packages
rm(list=ls()) # Clear the workspace
library(R2jags)

# Maths -------------------------------------------------------------------

# Description of the Bayesian model fitted in this file
# Notation:
# y_i = repsonse variable for observation t=i,..,N
# x_i = explanatory variable for obs i
# alpha, beta = intercept and slope parameters to be estimated
# sigma = residual standard deviation

# Likelihood:
# y[i] ~ N(alpha + beta * x[i], sigma^2)
# Prior
# alpha ~ N(0,100) - vague priors
# beta ~ N(0,100)
# sigma ~ U(0,10)

# Simulate data -----------------------------------------------------------

# Some R code to simulate data from the above model
n = 100
alpha = 2
beta = 3
sigma = 1
# Set the seed so this is repeatable
set.seed(123)
x = sort(runif(n, 0, 10)) # Sort as it makes the plotted lines neater
y = rnorm(n, mean = alpha + beta * x, sd = sigma)

# Also creat a plot
plot(x, y)
lines(x, alpha + beta * x)

# Jags code ---------------------------------------------------------------

# Jags code to fit the model to the simulated data

model_code = '
model
{
  # Likelihood
  for (i in 1:n) {
    y[i] ~ dnorm(fits[i], sigma^-2)
    fits[i] = alpha + beta * x[i]
  }

  # Priors
  alpha ~ dnorm(0, 100^-2)
  beta ~ dnorm(0, 100^-2)
  sigma ~ dunif(0, 10)
}
'

# Set up the data
model_data = list(n = n, y = y, x = x)

# Choose the parameters to watch
model_parameters =  c("fits")

# Run the model
model_run = jags(data = model_data,
                 parameters.to.save = model_parameters,
                 model.file=textConnection(model_code))

# Simulated results -------------------------------------------------------

# Compute the R-squared value from the fits
fits = model_run$BUGSoutput$mean$fits
R_squared = cor(y, fits)^2
