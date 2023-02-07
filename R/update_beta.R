update_beta <- function(alpha, beta, d, Z, mu_censor, gamma = 0){
  
  
  K <- nrow(beta)
  xi <- p_xi(alpha, Z)
  D <- p_D(d, beta, Z, mu_censor, gamma)
  
  #data for regression
  Y <- d / mu_censor
  #Y <- round(d / mu_censor, 0)
  dat <- as.data.frame(cbind(Y, Z))
  col_name <- colnames(dat)
  formule <- as.formula(paste(col_name[1], paste(col_name[-1], collapse = " + "), sep = " ~ "))
  
  #weight for regression
  tau_temp <- D * xi # n*k
  tau <- t(apply(tau_temp, 1, function(x) x/sum(x))) #n*k
  
  new_beta <- NULL
  
  for(k in 1 : K){
    weight <- tau[,k]
    #glm1 <- glm(formula = formule, data = dat, family = poisson, weights = weight)
    suppressWarnings(glm1 <- glm(formula = formule, data = dat, family = poisson, weights = weight))
    coef1 <- glm1$coefficients
    new_beta <- rbind(new_beta, coef1)
  }
  
  
  return(new_beta)
}