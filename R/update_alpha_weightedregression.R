#' @importFrom nnet multinom
update_alpha <- function(alpha, beta, d, Z, mu_censor, gamma = 0){
  #number of latent class
  K <- nrow(beta)
  #p_xi <- function(alpha, Z)
  xi <- p_xi(alpha, Z)

  #p_D <- function(d, beta, Z, mu_censor, gamma)
  D <- p_D(d, beta, Z, mu_censor, gamma)

  Z_long <- apply(Z, 2, function(x) rep(x,K))
  xi_long <- rep(factor(1:K), each = nrow(Z))
  #xi_long <- as.factor(xi_long)
  dat <- as.data.frame(cbind(xi_long, Z_long))
  dat[,1] <- as.factor(dat[,1])
  dat[,1] <- relevel(dat[,1], ref = "1")

  col_name <- colnames(dat)
  #weight for regression
  tau_temp <- D * xi # n*k
  tau <- t(apply(tau_temp, 1, function(x) x/sum(x)))

  weight <- as.vector(tau)

  formule <- as.formula(paste(col_name[1], paste(col_name[-1], collapse = " + "), sep = " ~ "))

  invisible(capture.output(new_alpha_regression <- nnet::multinom(formula = formule, data = dat, weights =  weight)))

  new_alpha <- summary(new_alpha_regression)$coefficients
  new_alpha <- rbind(0, new_alpha)
  new_alpha <- new_alpha[,-1]

  return(new_alpha)
}
