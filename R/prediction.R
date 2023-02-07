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