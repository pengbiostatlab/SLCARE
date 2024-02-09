#' @title Estimate P(D|xi, Z, C)
#' @description Estimate P(Di = di | xi = k, Zi, Ci).
#' @param event_num wide-format number of observed events per subject.
#' @param beta class specific parameters for recurrent event model.
#' @param Z wide-format variables of interest.
#' @param mu_censor estimated mu0(C), where C is a vector of wide-format censoring time (longest follow up time).
#' @param gamma individual frailty. 0 represents the frailty equals 1 and k represents the frailty follows gamma(k,k).
#' @return  A vector of estimated P(Di_di | xi = k, Zi, Ci)
#' @noRd
p_D <- function(event_num, beta, Z, mu_censor, gamma = 0) {
  Z1 <- cbind(1, Z)
  exp_Zbeta <- exp(Z1 %*% t(beta)) # n*(p+1) (p+1)*k
  event_num_value <- (event_num)[[1]]
  if (gamma == 0) {
    p_D <- apply(exp_Zbeta, 2, function(x) ((x * mu_censor)^event_num_value) * exp(-x * mu_censor) / factorial(event_num_value))
  } else {
    p_D <- apply(
      exp_Zbeta, 2,
      function(x) factorial(event_num_value + gamma - 1) / factorial(event_num_value) / factorial(gamma - 1) * (gamma^gamma) * (x * mu_censor)^event_num_value / (gamma + x * mu_censor)^(event_num_value + gamma)
    )
  }
  return(p_D)
}
