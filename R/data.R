#' @title Simulated dataset for demonstration
#' @description A simulated dataset perturbed from a real dataset with the following variables:
#' @format A data frame with 476 rows and 6 variables.
#' \describe{
#'   \item{id}{subjects identification}
#'   \item{start}{start time of the interval for the recurrent event.}
#'   \item{stop}{ending time of the interval for the recurrent event; when time origin is 0 this variable also marks the recurrence or terminal/censoring time.}
#'   \item{event}{recurrent event indicator; 1 if a recurrent event is observed.}
#'   \item{x1}{a binary baseline covariate.}
#'   \item{x2}{a continuous baseline covariate.}
#' }
#'
#' @usage data(SimData)
#' @docType data
#' @name SimData
#' @rdname SimData
NULL
