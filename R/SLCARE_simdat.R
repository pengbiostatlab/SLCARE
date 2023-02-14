#' Simulated dataset
#'
#' @description A dataset simulated from a real world dataset
#' \describe{
#'   \item{id}{subjects identification}
#'   \item{time}{time recored including event and longest followup time (censoring)}
#'   \item{event}{recurrent event indicator; 1 if a recurrent event is recorded}
#'   \item{x1}{a dummy baseline covariate}
#'   \item{x2}{a continuous baseline covariate range from 0 to 1}
#' }
#'
#' @usage data(SLCARE_simdat)
#' @docType data
#' @name SLCARE_simdat
#' @rdname SLCARE_simdat
#' @format A data frame with 478 rows and 5 variables.
#' @importFrom stats sd quantile relevel as.formula glm poisson
#' @importFrom utils capture.output

globalVariables(c("event", "ID", "time", "Count", "observed", "predicted", "mu0t", "par", "mu0_t_dat_par"))
