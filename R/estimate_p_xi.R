#' @title Estimate P(xi | Z, C)
#' @description Estimate P(xi = k | Zi, Ci)
#' @param alpha regression coefficient for multinomial logistic regression model (xi).
#' @param Z wide-format variables of interest.
#' @return A vector of estimated P(xi = k | Zi, Ci).
#' @noRd
p_xi <- function(alpha, Z) {
  exp_Zalpha <- exp(Z %*% t(alpha)) # n*p %*% p*k ==> n*k
  return(t(apply(exp_Zalpha, 1, function(x) x / sum(x))))
}
