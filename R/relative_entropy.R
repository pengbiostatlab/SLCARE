#' @title Calculate relative entropy
#' @description Calculate relative entropy for the selection of individual frailty and number of latent classes
#' @param alpha regression coefficient for multinomial logistic regression model
#' @param beta class specific parameters for recurrent model
#' @param d a vector of observed recurrent events for subjects of interest
#' @param Z a vector of time-independent corvariates
#' @param mu_censor a vector of estimated mu(C), where C is a vector of censoring time
#' @param gamma individual frailty. 0 represents the frailty equals 1 and k represents the frailty follows gamma(k,k)
#' @return a numerical number which measures relative entropy
entropy <- function(alpha, beta, d, Z, mu_censor, gamma = 0){
  #number of latent class
  K <- nrow(beta)
  n <- nrow(Z)
  xi <- p_xi(alpha, Z)
  D <- p_D(d, beta, Z, mu_censor, gamma)

  tau_temp <- D * xi # n*k
  tau <- t(apply(tau_temp, 1, function(x) x/sum(x)))

  #function 15
  entropy <- 1- ( sum(- tau * log(tau), na.rm = T) / (n*log(K)) )

  return(entropy)

}
