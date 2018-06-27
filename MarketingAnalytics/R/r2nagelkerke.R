#' @title Function to calculate Nagelkerke Index (pseudo R2)
#'
#' @description Longer description on how the function works. Texts may
#' be longer than one line. No problem at all.
#'
#' @param y numeric: vector of binary responses
#' @param yEstimate numeric: vector of estimated probabilities
#' @param tol numeric: ... further arguments passed to \code{\link{otherFunction}}
#' @return type: value, correction in case yEstimate equals 0 or 1 for
#' logarithm to be defined
#' Correction of estimated probabilities with value 0 to tol and 1 to 1-tol
#'
#' @export
#'
#' @examples
#' Some examples for using the function
#'
#'
R2Nagelkerke <- function(y, yEstimate, tol = 1e-8) {
  yEstimate <- pmax(tol, pmin(yEstimate, 1 - tol))
  n <- NROW(y)
  s <- sum(y)
  P <- s / n
  logLikelihoodNull <- s * log(P) + (n - s) * log(1 - P)
  LNull <- -2 * logLikelihoodNull
  logLikelihood <- sum(y * log(yEstimate / (1 - yEstimate)) + log(1 - yEstimate))
  LR <- -2 * logLikelihood
  return((1 - exp( - (LNull - LR) / n)) / (1 - exp(-LNull / n)))
}
