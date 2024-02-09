#' @title Semiparametric Latent Class Analysis for Recurrent Event - Point Estimate
#' @description Fit semiparametric latent class model for recurrent event and obtain the point estimate
#' @param alpha initial values for alpha for estimation procedure (multinomial logistic regression model). This should be NULL or a numeric matrix. 'NULL' refers to obtain initial value with the proposed method.
#' @param beta initial value for beta for estimation procedure (recurrent event model). This should be NULL or a numeric matrix. 'NULL' refers to obtain initial value with the proposed method.
#' @param data a long-format Data-frame.
#' @param id_col the unique identifier per subject.
#' @param start_col start time of the interval for the recurrent event.
#' @param stop_col ending time of the interval for the recurrent event.
#' @param event_col The status indicator, 1 = observed recurrent event.
#' @param formula A string specifying the variables of interest to be involved in the regression.
#' @param K number of latent classes.
#' @param gamma individual frailty. 0 represents the frailty equals 1 and k represents the frailty follows gamma(k,k).
#' @param max_epochs maximum iteration epochs for the estimation procedure.
#' @param conv_threshold converge threshold for the estimation procedure.
#' @return A list containing the following components:
#' \describe{
#' \item{alpha}{Point estimates for alpha}
#' \item{beta}{Point estimates for beta}
#' \item{convergeloss}{Converge loss in estimation procedure}
#' }
#' @noRd
SLCARE_fit <- function(alpha = NULL, beta = NULL, data = data, id_col = "id", start_col = "start", stop_col = "stop", event_col = "event", formula = "x1 + x2", K = NULL,
                       gamma = 0, max_epochs = 200, conv_threshold = 0.1) {
  dat_list <- PreprocessData(data = data, id_col = id_col, start_col = start_col, stop_col = stop_col, event_col = event_col, formula = formula)
  id_wide <- dat_list$id_wide
  id_long <- dat_list$id_long
  Z <- as.matrix(dat_list$Z)
  time_long <- dat_list$time_long
  censor_wide <- (dat_list$censor_wide)[[1]]
  censor_long <- dat_list$censor_long
  event_num <- dat_list$event_num

  if (is.numeric(alpha)) {
    init_alpha <- alpha
    init_beta <- beta
  } else {
    # obtain initials
    initial <- get_initial(data = data, K = K, id_col = id_col, start_col = start_col, stop_col = stop_col, event_col = event_col, formula = formula)
    init_alpha <- as.matrix(initial$ini_alpha)
    init_beta <- as.matrix(initial$ini_beta)
    alpha <- init_alpha
    beta <- init_beta
  }
  # K <- nrow(init_beta)
  mu_censor <- sapply(censor_wide, function(x) mu_t(time_long, censor_long, x))

  converged <- F
  epochs <- 0

  while (converged == F) {
    alpha_new <- update_alpha(alpha, beta, event_num, Z, mu_censor, gamma)
    beta_new <- update_beta(alpha, beta, event_num, Z, mu_censor, gamma)
    diff_alpha <- (alpha_new - alpha) / alpha
    diff_beta <- (beta_new - beta) / beta
    diff_alpha2 <- alpha_new - alpha
    diff_beta2 <- beta_new - beta
    loss1 <- max(abs(diff_alpha2))
    loss2 <- max(abs(diff_beta2))
    loss <- max(loss1, loss2)
    alpha <- alpha_new
    beta <- beta_new

    if (loss <= conv_threshold) {
      converged <- T
      rownames(alpha) <- paste0("class", c(1:K), sep = "")
      rownames(beta) <- paste0("class", c(1:K), sep = "")
      output <- list("alpha" = alpha, "beta" = beta, "convergeloss" = loss)
    } else {
      epochs <- epochs + 1
    }

    if (epochs >= max_epochs) {
      converged <- T
      rownames(alpha) <- paste0("class", c(1:K), sep = "")
      rownames(beta) <- paste0("class", c(1:K), sep = "")
      output <- list("alpha" = alpha, "beta" = beta, "convergeloss" = loss)
    }
  }
  return(output)
}
