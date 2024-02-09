#' @title Estimation algorithm - updating beta
#' @description Updating beta in estimation procedure. Updating beta by fitting "pseudo" weighted Poisson regression model.
#' @param alpha a matrix of alpha before updating - regression coefficient for multinomial logistic regression model (xi).
#' @param beta a matrix of beta before updating - class specific parameters for recurrent event model.
#' @param event_num wide-format number of observed events per subject.
#' @param Z wide-format variables of interest.
#' @param mu_censor estimated mu0(C), where C is a vector of wide-format censoring time (longest follow up time).
#' @param gamma individual frailty. 0 represents the frailty equals 1 and k represents the frailty follows gamma(k,k).
#' @return a matrix of updated beta - class specific parameters for recurrent event model.
#' @noRd
update_beta <- function(alpha, beta, event_num, Z, mu_censor, gamma = 0) {
  K <- nrow(beta)
  xi <- p_xi(alpha, Z)
  D <- p_D(event_num, beta, Z, mu_censor, gamma)
  # prepare data for regression
  Y <- (event_num / mu_censor)[[1]]
  # Y <- round(event_num / mu_censor, 0)
  X <- cbind(1, Z)
  # dat <- as.data.frame(cbind(Y, Z))
  # col_name <- colnames(dat)
  # formule <- as.formula(paste(col_name[1], paste(col_name[-1], collapse = " + "), sep = " ~ "))
  # weight for regression
  tau_temp <- D * xi # n*k
  tau <- t(apply(tau_temp, 1, function(x) x / sum(x))) # n*k
  new_beta <- NULL

  for (k in 1:K) {
    weight <- tau[, k]
    # suppressWarnings(glm1 <- glm(formula = formule, data = dat, family = poisson(), weights = weight))
    suppressWarnings(glm1 <- glm.fit(x = X, y = Y, family = poisson(), weights = weight))
    coef1 <- glm1$coefficients
    new_beta <- rbind(new_beta, coef1)
  }

  return(new_beta)
}
