#' @title Semiparametric Latent Class Analysis for Recurrent Event
#' @description Conduct Semiparametric Latent Class Analysis for Recurrent Event.
#' @param alpha initial values for alpha for estimation procedure. This should be NULL or a numberic matirx. NULL means obtain initial value with k-means.
#' @param beta initial value for beta for estimation procedure. This should be NULL or a numberic matirx. NULL means obtain initial value with k-means.
#' @param dat a data frame containing the data in the model
#' @param K number of latent classes
#' @param gamma individual frailty. 0 represents the frailty equals 1 and k represents the frailty follows gamma(k,k)
#' @param max_epoches maximum iteration epoches for estimation procedure
#' @param conv_threshold converge threshold for estimation procedure
#' @param boot bootstrap sample size
#' @return A list containing the following components:
#' \describe{
#' \item{alpha}{Point estimates for alpha}
#' \item{beta}{Point estimates for beta}
#' \item{convergeloss}{Converge loss in estimation procedure}
#' \item{PosteriorPrediction}{Posterior prediction for observed events for subjects of interest}
#' \item{EstimatedTau}{Posterior probability of latent class membership}
#' \item{ModelChecking}{Plot for model checking}
#' \item{Estimated_mu0t}{Plot for estimated mu0(t)}
#' \item{est_mu0()}{A function allows to calculate mu0(t) for specific time points}
#' \item{Estimated_Mean_Function}{Plot of estimated mean functions}
#' \item{RelativeEntropy}{Relative entropy}
#' \item{InitialAlpha}{Initial alpha for estimation procedure}
#' \item{InitialBeta}{Initial beta for estimation procedure}
#' }
#' If argument 'boot' is non-NULL, then SLCARE returns two additional components:
#' \describe{
#' \item{alpha_bootse}{Bootstrap standard error for alpha}
#' \item{beta_bootse}{Bootstrap standard error for beta}
#' }
#' @import dplyr tidyr ggplot2 reReg nnet
#' @export
#' @examples
#' data(SLCARE_simdat)
#' # Example 1: number of latent classes k = 2,
#' # By default, generate initial values in estimation procedure with K-means
#' model1 <- SLCARE(dat = SLCARE_simdat, K=2)
#' # contents of output
#' names(model1)
#' # point estimates
#' model1$alpha
#' model1$beta
#' # converge loss in estimation procedure
#' model1$convergeloss
#' # Posterior prediction
#' model1$PosteriorPrediction
#' # Posterior probability of latent class membership
#' model1$EstimatedTau
#' # model checking plot
#' model1$ModelChecking
#' # Plot of estimated \eqn(\mu_0 (t)) for all observed time
#' model1$Estimated_mu0t
#' # Estimated \eqn(\mu_0 (t))
#' # You may input multiple time points of interest
#' model1$est_mu0(c(100, 1000, 5000))
#' # Plot of estimated mean function
#' model1$Estimated_Mean_Function
#' # Relative entropy
#' model1$RelativeEntropy
#' # Initial values for estimation procedure
#' model1$InitialAlpha
#' model1$InitialBeta
#' # You can select initial value in estimation procedure manually
#' alpha <- matrix(c(0, 0, 0.5, -2, 2, -4),
#'                 nrow = 3, ncol = 2, byrow = TRUE)
#' beta <- matrix(c(2.5, -0.5, -0.3, 1.5, -0.2, -0.5,
#'                   2.5,  0.1, 0.2), nrow = 3 , ncol = 2+1 , byrow = TRUE)
#' model2 <- SLCARE(alpha, beta, dat = SLCARE_simdat)
#' # You can define individual frailty with gamma(p,p).
#' # Below is an example with manually defined initial value and frailty gamma(3,3)
#' model3 <- SLCARE(alpha, beta, dat = SLCARE_simdat, gamma = 3)
#' # You can use bootstrap for bootstrap standard error.
#' # Bootstrap sample size = 100 (time consuming procedure)
#' # model4 <- SLCARE(alpha, beta, dat = SLCARE_simdat, boot = 100)
#' # SLCARE() with "boot" argument will return to two additional contents:
#' # "alpha_bootse", "beta_bootse" which are Bootsrap standard errors.
#' # model4$alpha_bootse
#' # model4$beta_bootse

SLCARE <- function(alpha = NULL, beta = NULL, dat, K = NULL,
                       gamma = 0, max_epoches = 500, conv_threshold = 0.01, boot = NULL){

  #id_wide, d, Z, censor_wide,
  #id_long, time_long, censor_long
  dat_list <- PreprocessData(dat)
  id_wide <- as.data.frame(dat_list$id_wide)
  colnames(id_wide) <- c("ID")
  d <- dat_list$d
  Z <- as.matrix(dat_list$Z)
  censor_wide <- dat_list$censor_wide

  id_long <- as.data.frame(dat_list$id_long)
  colnames(id_long) <- c("ID")
  time_long <- dat_list$time_long
  censor_long <- dat_list$censor_long

  if( is.numeric(alpha) ){
    init_alpha <- alpha
    init_beta  <- beta
  } else {
    #obtain initial
    initial <- get_initial(dat, K)
    init_alpha <- as.matrix(initial$ini_alpha)
    init_beta <- as.matrix(initial$ini_beta)
    alpha <- init_alpha
    beta <- init_beta
  }

  K <- nrow(init_beta)

  mu_censor <- sapply(censor_wide, function(x) mu_t(time_long, censor_long, x))

  converged = F
  epoches = 0



  while(converged == F){
    alpha_new <- update_alpha(alpha, beta, d, Z, mu_censor, gamma)
    beta_new <- update_beta(alpha, beta, d, Z, mu_censor, gamma)

    diff_alpha <- (alpha_new - alpha)/ alpha
    diff_beta <- (beta_new - beta)/ beta


    diff_alpha2 <- alpha_new - alpha
    diff_beta2 <- beta_new - beta

    loss1 <- max(abs(diff_alpha2))
    loss2 <- max(abs(diff_beta2))

    loss <- max(loss1, loss2)

    alpha <- alpha_new
    beta <- beta_new


    if(loss <= conv_threshold){
      converged = T
      rownames(alpha) <- paste0("class", c(1:K), sep = "")
      rownames(beta) <- paste0("class", c(1:K), sep = "")
      output <- list("alpha" = alpha, "beta" = beta, "convergeloss" = loss)

    }else{
      epoches <- epoches + 1
    }

    if(epoches >= max_epoches){
      converged = T
      rownames(alpha) <- paste0("class", c(1:K), sep = "")
      rownames(beta) <- paste0("class", c(1:K), sep = "")
      output <- list("alpha" = alpha, "beta" = beta, "convergeloss" = loss)

    }
  }


  ##Bootstrap
  if( is.numeric(boot) )
  {
    n_subjects <- nrow(id_wide)
    #long_format <- cbind(id_long, time_long, censor_long)
    list_alpha_boot <- NULL
    list_beta_boot <- NULL

    for( i in 1:boot )
    {
      skip_to_next <- FALSE

      tryCatch(
        {
          boot_subject_id <- as.data.frame(sample(dat_list$id_wide, n_subjects, replace=T))
          colnames(boot_subject_id) <- c("ID")
          dat_boot_temp <- boot_subject_id |> left_join(dat, by = names(id_long))

          dat_boot <- dat_boot_temp |> group_by(ID, time) |>
            mutate(Count = row_number()) |>
            ungroup() |>
            mutate(ID = ifelse(Count > 1, paste0(ID, "BOOT", Count), ID)) |>
            select(-Count)


          # subject_index <- sample(1:n_subjects,n_subjects,replace=T)
          # id_wide_boot <- as.data.frame(id_wide[subject_index,])
          # names(id_wide_boot) <- names(id_wide)
          # d_boot <- d[subject_index]
          # Z_boot <- Z[subject_index,]
          # censor_wide_boot <- censor_wide[subject_index]
          # long_format_boot <- merge(id_wide_boot, long_format, by = names(id_long), all.x=TRUE)

          ####mu_censor_boot <- sapply(censor_wide_boot, function(x) mu_t(long_format_boot[2], long_format_boot[3], x))

          # output_boot <- SLCARE(output$alpha, output$beta,
          #                           id_wide_boot, d_boot, Z_boot, censor_wide_boot,
          #                           long_format_boot[1], long_format_boot[2], long_format_boot[3],
          #                           gamma, max_epoches = 200, conv_threshold = 0.1, boot = NULL)
          output_boot <- SLCARE(output$alpha, output$beta, dat_boot,
                              gamma, max_epoches = 200, conv_threshold = 0.1, boot = NULL)


          list_alpha_boot <- rbind(list_alpha_boot, as.vector(output_boot$alpha))
          list_beta_boot <- rbind(list_beta_boot, as.vector(output_boot$beta))
          #list_alpha_boot <- append(list_alpha_boot, list(output_boot$alpha))
          #list_beta_boot  <- append(list_beta_boot, list(output_boot$beta))

        },
        error = function(e) {skip_to_next <<- TRUE}
      )
      if(skip_to_next) { next }
    }



    beta_bootsd <- matrix(apply(list_beta_boot, 2, function(x) sd(x[quantile(x , 0.025) <= x & x <= quantile(x, 0.975)])) , nrow = K)
    alpha_bootsd <- matrix(apply(list_alpha_boot, 2, function(x) sd(x[quantile(x , 0.025) <= x & x <= quantile(x, 0.975)])) , nrow = K)
    colnames(alpha_bootsd) <- colnames(output$alpha)
    rownames(alpha_bootsd) <- rownames(output$alpha)
    colnames(beta_bootsd)  <- colnames(output$beta)
    rownames(beta_bootsd)  <- rownames(output$beta)

    output <- list("alpha" = output$alpha, "beta" = output$beta, "convergeloss" = output$convergeloss, "alpha_bootse" = alpha_bootsd, "beta_bootse" = beta_bootsd)

  }

  #posterior predict
  predict <- SLCA_predict(output$alpha, output$beta, d, Z, mu_censor, gamma)

  PosteriorPrediction <- cbind(id_wide, predict$PosteriorPredict)
  colnames(PosteriorPrediction) <- c("ID", "PosteriorPrediction")
  EstimatedTau <- cbind(id_wide, predict$tauhat)
  colnames(EstimatedTau) <- c("ID",paste0("class", c(1:K), sep = ""))
  output <- append(output, list("PosteriorPrediction" = PosteriorPrediction, "EstimatedTau" = EstimatedTau))

  #model checking
  modelcheckdat <- as.data.frame(cbind(d, round(predict$PosteriorPredict,0)))

  colnames(modelcheckdat) <- c("observed", "predicted")

  modelcheckplot <- ggplot(modelcheckdat, aes(x = observed, y = predicted)) +
                          geom_point() +
                          geom_jitter(width = max(modelcheckdat$observed)/20, height = max(modelcheckdat$observed)/20, alpha = 0.3, col = 'blue') +
                          geom_abline(intercept = 0, slope = 1) +
                          theme(aspect.ratio=1) +
                          ggtitle("Model Checking")
                          #coord_fixed(ratio = 1)

  output <- append(output, list("ModelChecking" = modelcheckplot))

  #est_mu0
  output$est_mu0 <- function(t)
    sapply(t, function(x) mu_t(time_long, censor_long, x))

  #plot mu_0(t)
  tseq <- seq(from = min(time_long), to = max(censor_wide), by = (max(censor_wide) - min(time_long))/200 )
  mu0_tseq <- sapply(tseq, function(x) mu_t(time_long, censor_long, x))

  mu0_t_dat <- as.data.frame(cbind(tseq, mu0_tseq))

  colnames(mu0_t_dat) <- c("t", "mu0t")

  estmu_plot <- ggplot(mu0_t_dat, aes(x = t, y = mu0t)) +
              #geom_smooth(se = FALSE) +
    geom_line(size = 1) +
    theme(aspect.ratio = 1) +
    ggtitle(expression(paste(Estimated~mu[0]~(t)))) +
    xlab("t") +
    ylab(expression(paste(mu[0]~(t))))


  output <- append(output, list("Estimated_mu0t" = estmu_plot))

  #estimated mean plot
  post_xi <- apply(predict$tauhat, 1, function(x) which.max(x))
  #post_xi_tau <- cbind(post_xi, predict$tauhat)
  tauexpzbeta <- apply(as.matrix(predict$tauhat) * exp(as.matrix(cbind(1, Z)) %*% t(as.matrix(output$beta))), 1, sum)
  xitauexpzbeta <- as.data.frame(cbind(post_xi, tauexpzbeta))

  class_par <- xitauexpzbeta |> group_by(post_xi)|> mutate(par = mean(tauexpzbeta)) |> select(post_xi, par) |> unique()

  estmean_crossingdat <- crossing(class_par, mu0_t_dat)
  estmean_crossingdat <- estmean_crossingdat |> mutate(mu0_t_dat_par = par * mu0t)
  estmean_crossingdat$class <- as.factor(estmean_crossingdat$post_xi)

  estmean_plot <- ggplot(estmean_crossingdat, aes(x = t, y = mu0_t_dat_par, colour = class)) +
    #geom_smooth(se = FALSE) +
    geom_line(size = 1) +
    theme(aspect.ratio = 1) +
    ggtitle("Estimated Mean Function Plot") +
    xlab("t") +
    ylab("Estimated Mean Function")

  output <- append(output, list("Estimated_Mean_Function" = estmean_plot))


  entropy <- entropy(output$alpha, output$beta, d, Z, mu_censor, gamma)

  output <- append(output, list("RelativeEntropy" = entropy))

  output <- append(output, list("InitialAlpha" = init_alpha, "InitialBeta" = init_beta))

  return(output)


}
