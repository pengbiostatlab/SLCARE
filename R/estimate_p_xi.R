p_xi <- function(alpha, Z){
  #K <- nrow(alpha)
  #n <- nrow(Z)
  exp_Zalpha <- exp(Z %*% t(alpha))  # n*p %*% p*k ==> n*k
  return(t(apply(exp_Zalpha, 1, function(x) x/sum(x))))
}