#' @title Posterior prediction for model checking
#' @description Predict numbers of recurrent events.
#' @param alpha estimated alpha - multinomial regression coefficients for latent class membership
#' @param beta estimated beta - class
#' @param d a vector of observed recurrent events for subjects of interest
#' @param Z a vector of time-independent corvariates
#' @param mu_censor a vector of estimated mu(C), where C is a vector of censoring time
#' @param gamma individual frailty. 0 represents the frailty equals 1 and k reprsents the frailty follows gamma(k,k)
#' @return A list containing the following components:
#' \tabular{ll}{
#'    \code{PosteriorPredict} \tab A vector of posterior prediction for observed events for subjects of interest \cr
#'    \tab \cr
#'    \code{tauhat} \tab A matrix of posterior probability of latent class membership \cr
#' }
SLCA_predict <- function(alpha, beta, d, Z, mu_censor, gamma = 0){

  K <- nrow(beta)
  xi <- p_xi(alpha, Z)
  D <- p_D(d, beta, Z, mu_censor, gamma)

  tau_temp <- D * xi # n*k
  tau <- t(apply(tau_temp, 1, function(x) x/sum(x)))

  Z1 <- cbind(1, Z)
  exp_Zbeta <- exp(Z1 %*% t(beta)) # n*(p+1) (p+1)*k

  prediction <- as.vector(t(apply(tau * exp_Zbeta * mu_censor, 1 , sum)))
  #names(prediction) <- c("prediction")

  result <- list("PosteriorPredict" = prediction, "tauhat" = tau)

  return(result)

}
