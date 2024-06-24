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

#' @title Follow-up of metastatic colorectal cancer patients: times of new lesions
#' appearance and death
#'
#' @description Randomly chosen 150 patients from the follow-up of the FFCD 2000-05
#' multicenter phase III clinical trial originally including 410 patients with
#' metastatic colorectal cancer randomized into two therapeutic strategies:
#' combination and sequential. The dataset contains times of observed
#' appearances of new lesions censored by a terminal event (death or
#' right-censoring) with baseline characteristics (treatment arm, age, WHO
#' performance status and previous resection).
#'
#'
#' @name colorectal
#' @docType data
#' @usage data(colorectal)
#' @format This data frame contains the following columns: \describe{
#' \item{id}{identification of each subject. Repeated for each recurrence}
#' \item{time0}{start of interval (0 or previous recurrence time)}
#' \item{time1}{recurrence or censoring time} \item{new.lesions}{Appearance of
#' new lesions status. 0: censsored or no event, 1: new lesions}
#' \item{treatment}{To which treatment arm a patient was allocated? 1:
#' sequential (S); 2: combination (C)} \item{age}{Age at baseline: 1: <50
#' years, 2: 50-69 years, 3: >69 years} \item{who.PS}{WHO performance status at
#' baseline: 1: status 0, 2: status 1, 3: status 2}
#' \item{prev.resection}{Previous resection of the primate tumor?  0: No, 1:
#' Yes} \item{state}{death indicator. 0: alive, 1: dead}
#' \item{gap.time}{interocurrence time or censoring time} }
#' @note This dataset was originally publicly available in frailtypack package (Rondeau et al. 2012).
#' Unfortunately the `frailtypack" is not available on CRAN at the time we create SLCARE.
#' We tentatively integrate this dataset to our package. The archive version of fraitypack on CRAN can be find at https://CRAN.R-project.org/package=frailtypack
#' @references M. Ducreux, D. Malka, J. Mendiboure, P.-L. Etienne, P. Texereau,
#' D. Auby, P. Rougier, M. Gasmi, M. Castaing, M. Abbas, P. Michel, D. Gargot,
#' A. Azzedine, C. Lombard- Bohas, P. Geoffroy, B. Denis, J.-P., Pignon,
#' L.,Bedenne, and O.  Bouche (2011). Sequential versus combination
#' chemotherapy for the treatment of advanced colorectal cancer (FFCD 2000-05):
#' an open-label, randomised, phase 3 trial.  \emph{The Lancet Oncology}
#' \bold{12}, 1032-44.
#' @keywords datasets
#' @name colorectal
#' @rdname colorectal
NULL

