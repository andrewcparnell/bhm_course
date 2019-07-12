# Model with 3 layer nested random effects


model_1 = '
model{
  for(i in 1:N) {
    coral_pc[i] ~ dnorm(intercept + slope * (wq[i] - mean(wq)), residual_sd^-2)
  }
  intercept ~ dnorm(0, 10^-2)
  slope ~ dnorm(0, 10^-2)
  residual_sd ~ dt(0, 10, 1)T(0, )
}
'

model_2 = '
model{
  for(i in 1:N) {
    coral_pc[i] ~ dnorm(intercept[reef_type[i]] +
        slope[reef_type[i]] * (wq[i] - mean(wq)),
        residual_sd^-2)
  }
  for(j in 1:N_reef_type) {
    intercept[j] ~ dnorm(mean_intercept, sd_intercept^-2)
    slope[j] ~ dnorm(slope_intercept, sd_slope^-2)
  }

  mean_intercept ~ dnorm(0, 10^-2)
  mean_slope ~ dnorm(0, 10^-2)
  residual_sd ~ dt(0, 10, 1)T(0, )
  sd_intercept ~ dt(0, 10, 1)T(0, )
  sd_slope ~ dt(0, 10, 1)T(0, )
}
'

model_3 = '
model{
  for(i in 1:N) {
    coral_pc[i] ~ dnorm(intercept[reef_type[i], season[i], site[i]] +
        slope[reef_type[i], season[i], site[i]] * (wq[i] - mean(wq)),
        residual_sd^-2)
  }
  for(j in 1:N_reef_type) {
    for(k in 1:N_season) {
      for(l in 1:N_site) {
        intercept[j,k,l] ~ dnorm(intercept_type_season[j,k],
            sd_site_intercept^-2)
        slope[j,k,l] ~ dnorm(slope_type_season[j,k],
            sd_slope_intercept^-2)
      }
      # intercept_type_season and slope_type_season are the key parameters!
      intercept_type_season[j,k] ~ dnorm(mean_intercept,
          sd_type_season_intercept^-2)
      slope_type_season[j,k] ~ dnorm(mean_slope,
          sd_type_season_slope^-2)
    }
  }

  mean_intercept ~ dnorm(0, 10^-2)
  mean_slope ~ dnorm(0, 10^-2)
  residual_sd ~ dt(0, 10, 1)T(0, )
  sd_site_intercept ~ dt(0, 10, 1)T(0, )
  sd_slope_intercept ~ dt(0, 10, 1)T(0, )
}
'

