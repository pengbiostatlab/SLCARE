#' @title Produce Plots for SLCARE
#' @description Generate cumulative baseline intensity function, estimated mean function, model checking plot for SLCARE.
#' @details
#' \code{SLCARE} provides visualization tools to depict the estimated functional model parameters and related functional quantities of interest.
#' These tools include:
#'
#' \bold{mu0:} estimated cumulative baseline intensity function.
#'
#' \bold{EstMeans:} estimated mean function plot - The crude estimates for the class-specific mean functions of recurrent events
#'
#' \code{SLCARE} also provides a tool to help assess the adequacy of the fitted model:
#'
#' \bold{ModelChecking:} model checking plot - A plot shows the comparison of the observed recurrent events versus the expected number of recurrent events. A major departure from the identity line may suggest a lack-of-fit of the assumed models.
#'
#' @param x an object of class \code{SLCARE}.
#' @param type the type of the plot.
#' @param ... other arguments.
#' @return A \code{ggplot} object.
#' @seealso [SLCARE()]
#' @export
plot.SLCARE <- function(x, type = c("ModelChecking", "mu0", "EstMeans"), ...) {
  if (!is.SLCARE(x)) stop("Response must be a `SLCARE` object.")
  type <- match.arg(type)
  plot <- switch(type,
    "ModelChecking" = plotModelChecking(x),
    "mu0" = plotCumBaseIntensity(x),
    "EstMeans" = plotEstMeans(x)
  )
  return(plot)
}


#' Produce Model Checking Plot
#' @param x an object of class \code{SLCARE}.
#' @return A \code{ggplot} object.
#' @noRd
plotModelChecking <- function(x) {
  modelcheckplot <- x$ModelChecking_gg +
    geom_point() +
    # alternative jitter plot
    # geom_jitter(width = max(modelcheckdat$observed)/20, height = max(modelcheckdat$observed)/20, alpha = 0.3, col = 'blue') +
    geom_abline(intercept = 0, slope = 1) +
    theme(aspect.ratio = 1) +
    ggtitle("Model Checking Plot") +
    expand_limits(x = 0, y = 0)
  return(modelcheckplot)
}

#' Plot Cumulative Intensity Function
#' @param x an object of class \code{SLCARE}.
#' @return A \code{ggplot} object.
#' @noRd
plotCumBaseIntensity <- function(x) {
  estmu_plot <- x$Estimated_mu0t_gg +
    # alternative smooth option
    # geom_smooth(se = FALSE) +
    geom_line(linewidth = 1) +
    theme(aspect.ratio = 1) +
    # ggtitle(expression(paste(Plot ~ of ~ hat(mu) ~ (t)))) +
    ggtitle("Estimated Cumulative Baseline Intensity Function Plot") +
    xlab("t") +
    # ylab(expression(paste(hat(mu) ~ (t)))) +
    ylab("Estimated Cumulative Baseline Intensity Function") +
    expand_limits(x = 0, y = 0) +
    theme(
      plot.title = element_text(size=8),       # Adjust the main title size
      axis.title.y = element_text(size=8)     # Adjust x-axis title size
    )
}
#' Plot Estimated Mean Function
#' @param x an object of class \code{SLCARE}.
#' @return A \code{ggplot} object.
#' @noRd
plotEstMeans <- function(x) {
  estmean_plot <- x$Estimated_Mean_Function_gg +
    # alternative smooth option
    # geom_smooth(se = FALSE) +
    geom_line(linewidth = 1) +
    theme(aspect.ratio = 1) +
    ggtitle("Estimated Mean Function Plot") +
    xlab("t") +
    ylab("Estimated Mean Function") +
    expand_limits(x = 0, y = 0)
}

#' @title Produce Plots for SLCARE (S4)
#' @description S4 method
#' @noRd
# setMethod("plot", signature(x="track", y="missing"), function(x, type = c("ModelChecking", "mu0", "EstMeans")){
#   if (!is.SLCARE(x)) stop("Response must be a `SLCARE` object.")
#   type <- match.arg(type)
#   plot <- switch(type,
#                  "ModelChecking" = plotModelChecking(x),
#                  "mu0" = plotCumBaseIntensity(x),
#                  "EstMeans" = plotEstMeans(x))
#   return(plot)
# })
NULL
