#' @title Semiparametric Latent Class Analysis for Recurrent Event
#' @description Fit semiparametric latent class model for recurrent event.
#' @details
#'
#' \bold{Model:}
#'
#' Suppose the recurrent events process is observed with the intensity function proposed in Zhao et al. (2022):
#' \deqn{\lambda _{i} (t) = \sum _{k = 1} ^{K} I (\xi _{i} = k) \times \lambda _{0} (t) \times W_{i} \times \eta _{0,k} \times \exp(\tilde{Z} _{i} ^{\top} \tilde{\beta} _{0,k}) }
#' where \eqn{K} is the number of latent classes in the whole population,
#' \eqn{\xi_i} denotes the unobserved latent class membership,
#' \eqn{\lambda _{0} (t)} is an unspecified, continuous,
#' nonnegative baseline intensity function shared by all latent classes,
#' \eqn{C_i} is the subject specific censoring time,
#' \eqn{\tilde{Z}_i} is the time-independent covariates,
#' \eqn{W_{i}} is a positive subject-specific latent variable independent of \eqn{(\xi_i, \tilde{Z}_i, C_i)}.
#'
#' The distribution of the latent class membership \eqn{\xi _{i}} is modeled by a logistic regression model:
#' \deqn{P(\xi _{i} = k | \tilde{Z} _{i}) = p_{k} (\alpha _{0} , \tilde{Z} _{i}) \doteq \frac{\exp(\tilde{Z} _{i} ^{\top} \alpha _{0,k})}{\sum_{k = 1}^{K}\exp(\tilde{Z} _{i} ^{\top} \alpha _{0,k}) } , \quad k = 1, \cdots, K }
#'
#' \code{SLCARE} is build for introducing a robust and flexible algorithm to carry out Zhao et al. (2022)'s latent class analysis method for recurrent event data described above.
#' The detailed discussion of the proposed estimation algorithms can be found in the paper "SLCARE: An R package for Semiparametric Latent Class Analysis of Recurrent Events" (in preparation).
#'
#' \bold{Initial Values:}
#'
#' The proposed estimating algorithm needs an input of initial values for \eqn{\hat{\beta}} and \eqn{\hat{\alpha}}.
#' \code{SLCARE} allows users to specify the initial values for the estimation algorithm by their own choice.
#' \code{SLCARE} also provide an automated initializer which obtains the initial values using
#' a combination of K-means clustering, multinomial regression and Wang et al. (2001)'s multiplicative intensity model.
#' The detailed discussion of the proposed estimation algorithms can be found in the paper "SLCARE: An R package for Semiparametric Latent Class Analysis of Recurrent Events" (in preparation).
#'
#' \bold{Specify the number of latent classes and individual frailty:}
#'
#' \code{SLCARE} allows the frailty distribution to be W = 1 or W follows a distribution that is parameterized as Gamma(k,k). These choices of frailty distributions cover a variety of density forms.
#' Suggested by Zhao et al. (2022), users can choose the distribution of individual frailty and the number of latent classes based on the model entropy provided by \code{SLCARE}.
#' An example of model selection can be found in the paper "SLCARE: An R package for Semiparametric Latent Class Analysis of Recurrent Events" (in preparation).
#'
#' @param formula A string specifying the variables of interest to be involved in the regression, with the format of "x1 + x2".
#' @param alpha initial values for alpha for estimation procedure (multinomial logistic regression model). This should be NULL or a numeric matrix. 'NULL' refers to obtain initial value with the proposed method.
#' @param beta initial value for beta for estimation procedure (recurrent event model). This should be NULL or a numeric matrix. 'NULL' refers to obtain initial value with the proposed method.
#' @param data a long-format Data-frame.
#' @param id_col the unique identifier per subject.
#' @param start_col start time of the interval for the recurrent event.
#' @param stop_col ending time of the interval for the recurrent event.
#' @param event_col The status indicator; 1 if a recurrent event is observed.
#' @param K number of latent classes.
#' @param gamma individual frailty. 0 represents the frailty equals 1 and k represents the frailty follows gamma(k,k).
#' @param max_epochs maximum iteration epochs for the estimation procedure.
#' @param conv_threshold converge threshold for the estimation procedure.
#' @param boot a numeric value specifies the number of bootstraps for variance estimation.
#' When \code{boot = NULL}, variance estimation will not be performed.
#' @export
#' @example inst/examples/SLCARE.R
SLCARE <- function(formula = "x1 + x2", alpha = NULL, beta = NULL, data = data, id_col = "id", start_col = "start", stop_col = "stop", event_col = "event", K = NULL,
                   gamma = 0, max_epochs = 500, conv_threshold = 0.01, boot = NULL) {
  if (is.null(K)) stop("Please specify the number of latent classes (K)")
  Call <- match.call()
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
      colnames(beta)[1] <- "(intercept)"
      output <- list("alpha" = alpha, "beta" = beta, "convergeloss" = loss, "call" = Call)
    } else {
      epochs <- epochs + 1
    }

    if (epochs >= max_epochs) {
      converged <- T
      rownames(alpha) <- paste0("class", c(1:K), sep = "")
      rownames(beta) <- paste0("class", c(1:K), sep = "")
      colnames(beta)[1] <- "(intercept)"
      output <- list("alpha" = alpha, "beta" = beta, "convergeloss" = loss, "call" = Call)
    }
  }

  ## Bootstrap
  if (is.numeric(boot)) {
    n_subjects <- nrow(id_wide)
    list_alpha_boot <- NULL
    list_beta_boot <- NULL

    for (i in 1:boot)
    {
      skip_to_next <- FALSE

      tryCatch(
        {
          boot_subject_id <- data.frame(ID = sample((id_wide)[[1]], n_subjects, replace = T))
          colnames(boot_subject_id) <- id_col
          dat_boot_temp <- boot_subject_id %>% left_join(data, by = id_col, relationship = "many-to-many")
          Count_boot <- NULL
          dat_boot <- dat_boot_temp %>%
            group_by(.data[[id_col]], .data[[start_col]]) %>%
            mutate(Count_boot = row_number()) %>%
            ungroup() %>%
            mutate(ID_boot = ifelse(Count_boot > 1, paste0(.data[[id_col]], "BOOT", Count_boot), .data[[id_col]])) %>%
            select(-Count_boot)

          output_boot <- SLCARE_fit(
            alpha = output$alpha, beta = output$beta, data = dat_boot,
            id_col = "ID_boot", start_col = start_col, stop_col = stop_col, event_col = event_col, formula = formula,
            K = K, gamma = gamma, max_epochs = 200, conv_threshold = 0.1
          )
          list_alpha_boot <- rbind(list_alpha_boot, as.vector(output_boot$alpha))
          list_beta_boot <- rbind(list_beta_boot, as.vector(output_boot$beta))
        },
        error = function(e) {
          skip_to_next <<- TRUE
        }
      )
      if (skip_to_next) {
        next
      }
    }

    # remove outlier
    beta_bootsd <- matrix(apply(list_beta_boot, 2, function(x) sd(x[quantile(x, 0.025) <= x & x <= quantile(x, 0.975)])), nrow = K)
    alpha_bootsd <- matrix(apply(list_alpha_boot, 2, function(x) sd(x[quantile(x, 0.025) <= x & x <= quantile(x, 0.975)])), nrow = K)
    colnames(alpha_bootsd) <- colnames(output$alpha)
    rownames(alpha_bootsd) <- rownames(output$alpha)
    colnames(beta_bootsd) <- colnames(output$beta)
    rownames(beta_bootsd) <- rownames(output$beta)
    # calculate p-value
    beta_pvalue <- 2 * pnorm(abs(output$beta / beta_bootsd), lower.tail = F)
    alpha_pvalue <- 2 * pnorm(abs(output$alpha / alpha_bootsd), lower.tail = F)
    output <- list("alpha" = output$alpha, "beta" = output$beta, "convergeloss" = output$convergeloss, "alpha_bootse" = alpha_bootsd, "beta_bootse" = beta_bootsd, "call" = Call, "alpha_pvalue" = alpha_pvalue, "beta_pvalue" = beta_pvalue)
  }

  # posterior predict
  predict <- predict_posterior(output$alpha, output$beta, event_num, Z, mu_censor, gamma)
  PosteriorPrediction <- data.frame(ID = (id_wide)[[1]], PosteriorPrediction = predict$PosteriorPredict)
  EstimatedTau <- cbind(id_wide, predict$tauhat)
  colnames(EstimatedTau) <- c("ID", paste0("class", c(1:K), sep = ""))
  output <- append(output, list("PosteriorPrediction" = PosteriorPrediction, "EstimatedTau" = EstimatedTau))

  # model checking
  observed <- NULL
  predicted <- NULL
  modelcheckdat <- data.frame(observed = (event_num)[[1]], predicted = predict$PosteriorPredict)
  modelcheckdat <- modelcheckdat %>% filter(observed != 0)

  modelcheck_gg <- ggplot(modelcheckdat, aes(x = observed, y = predicted))

  output <- append(output, list("ModelChecking_gg" = modelcheck_gg))

  # est_mu0
  output$est_mu0 <- function(t) {
    sapply(t, function(x) mu_t(time_long, censor_long, x))
  }

  # plot mu_0(t)
  mu0t <- NULL
  tseq <- seq(from = min(time_long), to = max(censor_wide), by = (max(censor_wide) - min(time_long)) / 200)
  mu0_tseq <- sapply(tseq, function(x) mu_t(time_long, censor_long, x))
  mu0_t_dat <- data.frame(t = tseq, mu0t = mu0_tseq)

  estmu_gg <- ggplot(mu0_t_dat, aes(x = t, y = mu0t))

  output <- append(output, list("Estimated_mu0t_gg" = estmu_gg))

  # estimated mean plot
  post_xi <- apply(predict$tauhat, 1, function(x) which.max(x))
  # post_xi_tau <- cbind(post_xi, predict$tauhat)
  tauexpzbeta <- apply(as.matrix(predict$tauhat) * exp(as.matrix(cbind(1, Z)) %*% t(as.matrix(output$beta))), 1, sum)
  xitauexpzbeta <- as.data.frame(cbind(post_xi, tauexpzbeta))
  par <- NULL
  class_par <- xitauexpzbeta %>%
    group_by(post_xi) %>%
    mutate(par = mean(tauexpzbeta)) %>%
    select(post_xi, par) %>%
    unique()
  estmean_crossingdat <- crossing(class_par, mu0_t_dat)
  mu0_t_dat_par <- NULL
  estmean_crossingdat <- estmean_crossingdat %>% mutate(mu0_t_dat_par = par * mu0t)
  estmean_crossingdat$class <- as.factor(estmean_crossingdat$post_xi)

  estmean_gg <- ggplot(estmean_crossingdat, aes(x = t, y = mu0_t_dat_par, colour = class))

  output <- append(output, list("Estimated_Mean_Function_gg" = estmean_gg))

  entropy <- entropy(output$alpha, output$beta, event_num, Z, mu_censor, gamma)

  output <- append(output, list("RelativeEntropy" = entropy))

  output <- append(output, list("InitialAlpha" = init_alpha, "InitialBeta" = init_beta))

  output <- structure(output, class = "SLCARE")

  return(output)
}


#' @title Is the object from the SLCARE class?
#' @description \code{TRUE} if the specified object is from the \code{\link{SLCARE}} class, \code{FALSE} otherwise.
#' @param x An \code{R} object.
#' @return A logical value.
#' @noRd
is.SLCARE <- function(x) {
  is(x, "SLCARE")
}

#' @title An S4 class to represent SLCARE object
#' @noRd
# setClass("SLCARE", ...)
NULL
