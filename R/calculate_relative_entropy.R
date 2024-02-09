#' @title Calculate relative entropy
#' @description Calculate relative entropy for model selection (individual frailty and number of latent classes).
#' @param alpha a matrix of alpha before updating - regression coefficient for multinomial logistic regression model (xi).
#' @param beta a matrix of beta before updating - class specific parameters for recurrent event model.
#' @param event_num wide-format number of observed events per subject.
#' @param Z wide-format variables of interest.
#' @param mu_censor estimated mu0(C), where C is a vector of wide-format censoring time (longest follow up time).
#' @param gamma individual frailty. 0 represents the frailty equals 1 and k represents the frailty follows gamma(k,k).
#' @return a numerical number which measures the relative entropy.
#' @noRd
entropy <- function(alpha, beta, event_num, Z, mu_censor, gamma = 0) {
  K <- nrow(beta)
  n <- nrow(Z)
  xi <- p_xi(alpha, Z)
  D <- p_D(event_num, beta, Z, mu_censor, gamma)
  tau_temp <- D * xi # n*k
  tau <- t(apply(tau_temp, 1, function(x) x / sum(x)))
  entropy <- 1 - (sum(-tau * log(tau), na.rm = T) / (n * log(K)))
  return(entropy)
}
