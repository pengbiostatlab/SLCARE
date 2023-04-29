#' @title Estimation algorithm - updating beta
#' @description Updating beta in estimation procedure. Updating beta by fitting "pseudo" weighted Poisson regression model
#' @param alpha a matrix of alpha before updating - regression coefficient for multinomial logistic regression model
#' @param beta a matrix of beta before updating - class specific parameters for recurrent model
#' @param d a vector of observed recurrent events for subjects of interest
#' @param Z a vector of time-independent corvariates
#' @param mu_censor a vector of estimated mu(C), where C is a vector of censoring time
#' @param gamma individual frailty. 0 represents the frailty equals 1 and k represents the frailty follows gamma(k,k)
#' @return a matrix of updated beta - class specific parameters for recurrent model
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
