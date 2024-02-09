#' @title Print Results for SLCARE
#' @description Print point estimates (Est), bootstrap standard error estimates (SE), initial estimates for the estimation algorithm (Init), convergence criterion (ConvergeLoss), latent class membership probability (ClassProb), predicted number of recurrent events (PostPredict), relative entropy of the fitted model (Entropy), p-value (pvalue) for SLCARE.
#' @param x an object of class \code{SLCARE}.
#' @param type the type of the output.
#' @param ... other arguments.
#' @seealso [SLCARE()]
#' @export
print.SLCARE <- function(x, type = c("Est", "SE", "Init", "ConvergeLoss", "ClassProb", "PostPredict", "Entropy", "PValue"), ...) {
  if (!is.SLCARE(x)) stop("Response must be a `SLCARE` object.")
  type <- match.arg(type)
  print <- switch(type,
    "Est" = GetEst(x),
    "SE" = GetSE(x),
    "Init" = GetInit(x),
    "ConvergeLoss" = GetConvLoss(x),
    "ClassProb" = GetMembership(x),
    "PostPredict" = GetPostPred(x),
    "Entropy" = GetEntropy(x),
    "PValue" = GetPValue(x)
  )
  return(print)
}

#' Print point estimates (Est)
#' @param x an object of class \code{SLCARE}.
#' @noRd
GetEst <- function(x) {
  Est <- list("beta" = x$beta, "alpha" = x$alpha)
  return(Est)
}

#' Print bootstrap standard error estimates (SE)
#' @param x an object of class \code{SLCARE}.
#' @noRd
GetSE <- function(x) {
  if (is.null(x$alpha_bootse)) stop("Please conduct bootstrapping by specify 'boot' argument in SLCARE()")
  SE <- list("beta" = x$beta_bootse, "alpha" = x$alpha_bootse)
  return(SE)
}

#' Print p-values (PValue)
#' @param x an object of class \code{SLCARE}.
#' @noRd
GetPValue <- function(x) {
  PValue <- list("beta" = x$beta_pvalue, "alpha" = x$alpha_pvalue)
  return(PValue)
}

#' Print initial estimates for the estimation algorithm (Init)
#' @param x an object of class \code{SLCARE}.
#' @noRd
GetInit <- function(x) {
  Init <- list("beta" = x$InitialBeta, "alpha" = x$InitialAlpha)
  return(Init)
}

#' Print convergence criterion (ConvergeLoss)
#' @param x an object of class \code{SLCARE}.
#' @noRd
GetConvLoss <- function(x) {
  Loss <- x$convergeloss
  return(Loss)
}

#' Print latent class membership probability (ClassProb)
#' @param x an object of class \code{SLCARE}.
#' @noRd
GetMembership <- function(x) {
  Membership <- x$EstimatedTau
  return(Membership)
}

#' Print predicted number of recurrent events (PostPredict)
#' @param x an object of class \code{SLCARE}.
#' @noRd
GetPostPred <- function(x) {
  PostPred <- x$PosteriorPrediction
  return(PostPred)
}

#' Print relative entropy of the fitted model (Entropy)
#' @param x an object of class \code{SLCARE}.
#' @noRd
GetEntropy <- function(x) {
  Entropy <- x$RelativeEntropy
  return(Entropy)
}

#' @title Summary Results for SLCARE
#' @description Summary results for \code{SLCARE} object including regression coefficients, corresponding standard error estimates and relative entropy of the fitted model.
#' @param object an object of class \code{SLCARE}.
#' @param digits minimal number of significant digits.
#' @param ... other arguments.
#' @export
#' @seealso [SLCARE()]
summary.SLCARE <- function(object, digits = 3, ...) {
  if (!is.SLCARE(object)) stop("Response must be a `SLCARE` object.")
  # Call:
  if (!is.null(cl <- object$call)) {
    cat("Call:\n")
    dput(cl, control = NULL)
  }
  # Coefficients:
  # cat("\nCoefficients:\n")
  cat("\nCoefficients for Beta:\n")
  print(object$beta, digits = digits)
  if (!is.null(object$beta_bootse)) {
    cat("\nStd. Errors for Beta:\n")
    print(object$beta_bootse, digits = digits)
    cat("\nP. Values for Beta:\n")
    print(object$beta_pvalue, digits = digits)
  }
  cat("\nCoefficients for Alpha:\n")
  print(object$alpha, digits = digits)
  if (!is.null(object$beta_bootse)) {
    cat("\nStd. Errors for Alpha:\n")
    print(object$alpha_bootse, digits = digits)
    cat("\nP. Values for Alpha:\n")
    print(object$alpha_pvalue, digits = digits)
  }
  # Relative Entropy:
  cat("\nRelative Entropy:", format(object$RelativeEntropy, digits = digits), "\n")
  invisible(object)
}

#' @title Predict Results for SLCARE
#' @description Calculate the posterior predicted number of recurrent events.
#' @param object an object of class \code{SLCARE}.
#' @param integer logicals. If `FALSE` (the default value), the function returns to float numbers.
#' @param ... other arguments.
#' @seealso [SLCARE()]
#' @export
predict.SLCARE <- function(object, integer = FALSE, ...) {
  if (!is.SLCARE(object)) stop("Response must be a `SLCARE` object.")
  predict <- GetPostPred(object)
  PosteriorPrediction <- NULL
  if (integer) predict <- predict %>% mutate(PosteriorPrediction = floor(PosteriorPrediction))
  return(predict)
}
