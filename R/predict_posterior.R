#' @title Posterior prediction for model checking
#' @description Predict numbers of recurrent events.
#' @param alpha a matrix of alpha before updating - regression coefficient for multinomial logistic regression model (xi).
#' @param beta a matrix of beta before updating - class specific parameters for recurrent event model.
#' @param event_num wide-format number of observed events per subject.
#' @param Z wide-format variables of interest.
#' @param mu_censor estimated mu0(C), where C is a vector of wide-format censoring time (longest follow up time).
#' @param gamma individual frailty. 0 represents the frailty equals 1 and k represents the frailty follows gamma(k,k).
#' @return A list containing the following components:
#' \tabular{ll}{
#'    \code{PosteriorPredict} \tab A vector of posterior predicted events for subjects of interest. \cr
#'    \tab \cr
#'    \code{tauhat} \tab A matrix of posterior probability of latent class membership. \cr
#' }
#' @noRd
predict_posterior <- function(alpha, beta, event_num, Z, mu_censor, gamma = 0) {
  xi <- p_xi(alpha, Z)
  D <- p_D(event_num, beta, Z, mu_censor, gamma)
  tau_temp <- D * xi # n*k
  tau <- t(apply(tau_temp, 1, function(x) x / sum(x)))
  Z1 <- cbind(1, Z)
  exp_Zbeta <- exp(Z1 %*% t(beta)) # n*(p+1) (p+1)*k
  prediction <- as.vector(t(apply(tau * exp_Zbeta * mu_censor, 1, sum)))
  result <- list("PosteriorPredict" = prediction, "tauhat" = tau)
  return(result)
}
