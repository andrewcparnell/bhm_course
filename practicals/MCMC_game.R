# MCMC game

# Likelihood is:
# y[i] ~ normal(alpha, sigma^2)
# Priors are:
# alpha ~ normal(0, 2^2)
# sigma ~ uniform(0, 5)

rnorm(1, mean = 0, sd = 2)
runif(1, 0, 5)


# Data:
y = c(4.7, 2.1, 5.2, 0.7)

# Likelihood score
alpha = 2
sigma = 1
dnorm(y, mean = alpha, sd = sigma)
prod(dnorm(y, mean = alpha, sd = sigma))
dnorm(y, mean = alpha, sd = sigma, log = TRUE)
sum(dnorm(y, mean = alpha, sd = sigma, log = TRUE))
exp(sum(dnorm(y, mean = alpha, sd = sigma, log = TRUE)))

# Prior score
dnorm(alpha, 0, 2, log=TRUE) + dunif(sigma, 0, 5, log=TRUE)

# Total log posterior score
sum(dnorm(y, mean = alpha, sd = sigma, log = TRUE)) + dnorm(alpha, 0, 2, log=TRUE) + dunif(sigma, 0, 5, log=TRUE)

# Proper MCMC version
iter = 1000
alpha = 1
sigma = 2
par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=-.01,las=1)
plot(1,alpha,xlim=c(1,iter), ylim = c(-2,10), col = 'blue', ylab = 'Parameter value', xlab = 'Iterations')
points(1,sigma, col = 'red')
legend('topleft', pch = 1, legend = c('alpha','sigma'), col = c('blue','red'), bty = 'n')

for(i in 1:iter) {
  alpha_new = rnorm(1, 0, 1)
  sigma_new = runif(1, 0, 5)

  post_score_new = sum(dnorm(y, mean = alpha_new, sd = sigma_new, log = TRUE)) + dnorm(alpha, 0, 2, log=TRUE) + dunif(sigma, 0, 5, log=TRUE)
  post_score_old = sum(dnorm(y, mean = alpha, sd = sigma, log = TRUE)) + dnorm(alpha, 0, 2, log=TRUE) + dunif(sigma, 0, 5, log=TRUE)

  U = runif(1)
  if(U < exp(post_score_new - post_score_old)) {
    alpha = alpha_new
    sigma = sigma_new
  }
  points(i,alpha, col='blue')
  points(i,sigma, col='red')
  Sys.sleep(0.2)

}


