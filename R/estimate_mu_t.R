#' @title estimate mu(t)
#' @description Estimate mu0 with the Nelson-Aalen type estimator under the assumed multiplicative intensity modeling of recurrent events
#' @param time_long long format time - events (excluding censoring time)
#' @param censor_long long format censoring time (longest follow up time)
#' @param t time of interest
#' @return estimated mu(t)
mu_t <- function(time_long, censor_long, t){
  #exp(t=0) = 1
  mu_0 <- 1
  #filter obs_time > t, remove 0 event patient since they do not contribute to this formula
  list <- !is.na(time_long)
  time_long <- time_long[list]
  censor_long <- censor_long[list]
  time_t <- time_long[time_long >= t]
  n_time_t <- length(time_t)

  if(n_time_t >= 1){
    nn = numeric(n_time_t)
    for(i in 1 : n_time_t){
      nn[i] <- sum( (time_t[i] <= censor_long) * (time_t[i] >= time_long) )
    }

    mu_0 = exp( -sum(1/nn) )
  }

  if(mu_0 == 0){
    mu_0 = 1e-6
  }
  return(mu_0)
}
