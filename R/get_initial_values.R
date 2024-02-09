#' @title Get initial values for the estimation procedure.
#' @description Get initial values for alpha by: 1. assign class membership to all subjects with Kmeans, then fit the multinomial regression to obtain alpha.
#' Obtain initial values for beta by: fitting the multiplicative intensity models by Wang et al. (2001) using the reReg() function, stratified by the latent class membership assigned by Kmeans.
#' @param data a long-format Data-frame.
#' @param K number of latent classes.
#' @param id_col the unique identifier per subject.
#' @param start_col start time of the interval for the recurrent event.
#' @param stop_col ending time of the interval for the recurrent event.
#' @param event_col The status indicator; 1 if a recurrent event is observed.
#' @param formula A string specifying the variables of interest to be involved in the regression.
#' @return A list of matrices containing the following components:
#' \tabular{ll}{
#'    \code{init_alpha} \tab A matrix of initial alpha \cr
#'    \tab \cr
#'    \code{init_beta} \tab A matrix of initial beta \cr
#' }
#' @noRd

get_initial <- function(data = data, K = 2, id_col = "id", start_col = "start", stop_col = "stop", event_col = "event", formula = "x1 + x2") {
  dat_list <- PreprocessData(data = data, id_col = id_col, start_col = start_col, stop_col = stop_col, event_col = event_col, formula = formula)
  id_wide <- dat_list$id_wide
  Z_long <- dat_list$Z_long
  id_long <- dat_list$id_long
  time_long <- dat_list$time_long
  censor_wide <- dat_list$censor_wide
  event_num <- dat_list$event_num
  Z <- dat_list$Z
  colnames(id_wide) <- "ID"
  colnames(id_long) <- "ID"
  # k-means
  Zdcensor <- cbind(Z, event_num, censor_wide)
  Kmeans_output <- kmeans(Zdcensor, centers = K, nstart = 25)
  random_xi <- data.frame(random_xi = Kmeans_output$cluster)
  # Fit Multinomial Log-linear Models
  dat <- as.data.frame(cbind(random_xi, id_wide, Z))
  dat[, 1] <- as.factor(dat[, 1])
  dat[, 1] <- relevel(dat[, 1], ref = "1")
  col_name <- colnames(dat)
  formule <- as.formula(paste(col_name[1], paste(col_name[-c(1, 2)], collapse = " + "), sep = " ~ "))
  invisible(capture.output(alpha_regression <- multinom(formula = formule, data = dat)))
  ini_alpha <- summary(alpha_regression)$coefficients
  ini_alpha <- rbind(0, ini_alpha)
  ini_alpha <- as.matrix(as.data.frame(ini_alpha[, -1]))
  colnames(ini_alpha) <- c(colnames(Z))
  # Fit Wang's Semiparametric Regression Model
  ddat1 <- cbind(time_long, id_long, Z_long)
  ddat2 <- cbind(censor_wide, id_wide, Z)
  colnames(ddat1)[1] <- "TIME"
  colnames(ddat2)[1] <- "TIME"
  ID <- NULL
  ddat <- rbind(ddat1, ddat2) %>% arrange(ID)
  col_name <- colnames(Z_long)
  formule <- as.formula(paste("Recur(TIME, id = ID )", paste(col_name, collapse = " + "), sep = " ~ "))
  ini_beta <- NULL
  for (i in 1:K) {
    subgroup_id <- dat %>%
      filter(random_xi == i) %>%
      select(ID)
    subddat <- ddat %>% semi_join(subgroup_id, by = "ID")
    fit_sub <- reReg(formule, data = subddat, model = "cox", B = 0)
    beta_sub <- c(fit_sub$log.muZ, fit_sub$par1)
    ini_beta <- rbind(ini_beta, beta_sub)
  }
  colnames(ini_beta) <- c("(intercept)", colnames(Z))
  rownames(ini_alpha) <- paste0("class", c(1:K), sep = "")
  rownames(ini_beta) <- paste0("class", c(1:K), sep = "")
  return(list(ini_alpha = ini_alpha, ini_beta = ini_beta))
}
