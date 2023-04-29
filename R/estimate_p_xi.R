#' @title Estimate P(xi | Z, C)
#' @description estimate P(xi = k | Zi, Ci)
#' @param alpha regression coefficient for multinomial logistic regression model (xi)
#' @param Z a vector of time-independent corvariates
#' @return a vector of estimated P(xi = k | Zi, Ci)
p_xi <- function(alpha, Z){
  #K <- nrow(alpha)
  #n <- nrow(Z)
  exp_Zalpha <- exp(Z %*% t(alpha))  # n*p %*% p*k ==> n*k
  return(t(apply(exp_Zalpha, 1, function(x) x/sum(x))))
}
