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