#' @title Semiparametric Latent Class Analysis for Recurrent Event - Point Estimate
#' @description Fit semiparametric latent class model for recurrent event and obtain the point estimate
#' @param formula a string specifying the variables of interest to be involved in the regression, with the format of "x1 + x2".
#' @param alpha initial estimate for alpha in the estimation procedure (multinomial logistic regression model). This should be NULL (default) or a numeric matrix. 'NULL' represents the initial estimate for alpha resulted from the automated initializer.
#' @param beta initial estimate for beta in the estimation procedure (recurrent event model). This should be NULL (default) or a numeric matrix. 'NULL' represents the initial estimate for beta resulted from the automated initializer.
#' @param data a long-format Dataframe, with the format similar to Simdata (a package build-in dataset).
#' @param id_col name of the column that includes subject identifiers.
#' @param start_col name of the column that records the start time of each at-risk time interval.
#' @param stop_col name of the column that records the start time of each at-risk time interval.
#' @param event_col name of the column that indicates whether a recurrent event is observed or not (i.e, 1=observed; 0=otherwise).
#' @param K pre-determined number of latent classes.
#' @param gamma parameter that indicates the distribution of frailty W. The default is 0 which indicates the model holds without the subject-specific frailty (i.e., W = 1), gamma = k indicates that W follows the Gamma(k, k) distribution.
#' @param max_epochs maximum number of iterations for the estimation algorithm.
#' @param conv_threshold convergence threshold for the estimation algorithm.
#' @return A list containing the following components:
#' \describe{
#' \item{alpha}{Point estimates for alpha}
#' \item{beta}{Point estimates for beta}
#' \item{convergeloss}{Converge loss in estimation procedure}
#' }
#' @noRd
SLCARE_fit <- function(formula = "x1 + x2", alpha = NULL, beta = NULL, data = data, id_col = "id", start_col = "start", stop_col = "stop", event_col = "event", K = NULL,
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
