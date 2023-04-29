#' @title Obtain initial values for estimation procedure.
#' @description Obtain initial value for alpha by: 1. assign class membership to all subjects with Kmeans, then fit the multinomial regression to obtain alpha.
#' Obtain initial value for beta by fitting the multiplicative intensity model studies by Wang et al. (2001) using the reReg() function, stratified by the latent class membership assigned by Kmeans.
#' @param dat a data frame containing the data in the model
#' @param K number of latent classes
#' @return A list containing the following components:
#' \tabular{ll}{
#'    \code{ini_alpha} \tab A matrix of initial alpha \cr
#'    \tab \cr
#'    \code{ini_beta} \tab A matrix of initial beta \cr
#' }
get_initial <- function(dat, K){

  dat_list <- PreprocessData(dat)
  ID_wide <- as.data.frame(dat_list$id_wide)
  Z_long <- dat_list$Z_long
  ID_long <- as.data.frame(dat_list$id_long)
  time_long <- dat_list$time_long
  censor <- dat_list$censor_wide
  d <- dat_list$d
  Z <- dat_list$Z
  #ID_wide, Z_long, ID_long, time_long, censor, d

  #get initial value
  Zdcensor <- cbind(Z, d, censor)
  Kmeans_output <- stats::kmeans(Zdcensor, center = K, nstart = 25)

  random_xi <- Kmeans_output$cluster


  colnames(ID_wide) <- "ID"
  dat <- as.data.frame(cbind(random_xi, ID_wide, Z))
  dat[,1] <- as.factor(dat[,1])
  dat[,1] <- relevel(dat[,1], ref = "1")
  col_name <- colnames(dat)
  formule <- as.formula(paste(col_name[1], paste(col_name[-c(1,2)], collapse = " + "), sep = " ~ "))

  #alpha_regression <- nnet::multinom(formula = formule, data = dat)
  #suppressMessages(alpha_regression <- nnet::multinom(formula = formule, data = dat))
  invisible(capture.output(alpha_regression <- nnet::multinom(formula = formule, data = dat)))

  ini_alpha <- summary(alpha_regression)$coefficients
  ini_alpha <- rbind(0, ini_alpha)
  ini_alpha <- as.matrix(ini_alpha[,-1])

  # get initial beta use Wang's method

  colnames(ID_long) <- "ID"
  #ddat <- cbind(ID_long, time_long, Z_long)
  ddat1 <- cbind(time_long, ID_long, Z_long)
  ddat2 <- cbind(censor, ID_wide, Z)
  colnames(ddat1)[1] <- "TIME"
  colnames(ddat2)[1] <- "TIME"

  ddat <- rbind(ddat1, ddat2) |> arrange(ID)

  col_name <- colnames(Z_long)
  formule <- as.formula(paste("Recur(TIME, id = ID )" , paste(col_name, collapse = " + "), sep = " ~ "))

  ini_beta <- NULL

  for (i in 1:K) {
    subgroup_id <- dat %>% filter(random_xi == i) %>% select(ID)
    subddat <- ddat %>% semi_join(subgroup_id, by = "ID")
    fit_sub <- reReg::reReg(formule, data = subddat, model = "cox", B = 0)
    beta_sub <- c(fit_sub$log.muZ, fit_sub$par1)
    ini_beta <- rbind(ini_beta, beta_sub)
  }

  colnames(ini_beta) <- c("intercept", colnames(ini_alpha))

  rownames(ini_alpha) <- paste0("class", c(1:K), sep = "")
  rownames(ini_beta) <- paste0("class", c(1:K), sep = "")


  return(list(ini_alpha = ini_alpha, ini_beta = ini_beta))
}
