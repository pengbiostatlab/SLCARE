#' @title Estimation algorithm - updating alpha
#' @description Updating alpha in estimation procedure. Updating alpha by fitting a weighted multinomial regression.
#' @param alpha a matrix of alpha before updating - regression coefficient for multinomial logistic regression model (xi).
#' @param beta a matrix of beta before updating - class specific parameters for recurrent event model.
#' @param event_num wide-format number of observed events per subject.
#' @param Z wide-format variables of interest.
#' @param mu_censor estimated mu0(C), where C is a vector of wide-format censoring time (longest follow up time).
#' @param gamma individual frailty. 0 represents the frailty equals 1 and k represents the frailty follows gamma(k,k)
#' @return a matrix of updated alpha - regression coefficient for multinomial logistic regression model (xi).
#' @noRd
update_alpha <- function(alpha, beta, event_num, Z, mu_censor, gamma = 0) {
  K <- nrow(beta)
  xi <- p_xi(alpha, Z)
  D <- p_D(event_num, beta, Z, mu_censor, gamma)
  Z_rep <- apply(Z, 2, function(x) rep(x, K))
  xi_long <- rep(factor(1:K), each = nrow(Z))
  dat <- as.data.frame(cbind(xi_long, Z_rep))
  dat[, 1] <- as.factor(dat[, 1])
  dat[, 1] <- relevel(dat[, 1], ref = "1")
  col_name <- colnames(dat)
  # weight for regression
  tau_temp <- D * xi # n*k
  tau <- t(apply(tau_temp, 1, function(x) x / sum(x)))
  weight <- as.vector(tau)
  formule <- as.formula(paste(paste(col_name[1], paste(col_name[-1], collapse = " + "), sep = " ~ "), '-1'))
  invisible(capture.output(new_alpha_regression <- multinom(formula = formule, data = dat, weights = weight)))
  new_alpha <- summary(new_alpha_regression)$coefficients
  new_alpha <- rbind(0, new_alpha)
  new_alpha <- as.matrix(as.data.frame(new_alpha))
  colnames(new_alpha) <- c(colnames(Z))

  return(new_alpha)
}
