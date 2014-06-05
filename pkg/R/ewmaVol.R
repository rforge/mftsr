# ewmaVol.R
#' Exponentially weighted moving average (EWMA) volatility estimation
#' 
#' Compute RiskMetrics type EWMA volatility estimator for a vector of returns.
#' @param r Any univariate data object that can be coerced to an xts object.
#' @param lambda Scalar exponential decay parameter. Must be between 0 and 1. If \code{lambda} is
#'   \code{NULL} then \code{half.life} must be specified and the value of \code{lambda} is computed from the 
#'   the value of \code{half.life} using \code{lambda = exp(log(0.5)/half.life)}.
#' @param half.life Scalar half-life defined as time lag at which the exponential weights decay by one half. 
#'   If \code{lambda} is not \code{NULL} then \code{half.life} is ignored and is computed internally from 
#'   the given value of \code{lambda} using \code{log(0.5)/log(lambda)}. . 
#' @param overlap  Integer value giving the aggregation period for overlapping returns. For example, if \code{r}
#'   represents daily returns and \code{overlap=5} then \code{r} is converted to a daily time series of 5-day
#'   overlapping returns. Default value is \code{1}.
#' @param demean  Character string indicating the method used to de-mean the returns. Valid choices are 
#'   \code{"none"} for no de-meaning; \code{"sample"} for the sample mean; and \code{"ewma"} for an EWMA
#'   estimate of the mean. For daily or weekly returns the mean is typically assumed to be zero. 
#'   For longer horizon returns, the mean is typically not zero. Default value is \code{"none"}.
#'   
#' @details
#' The EWMA variance estimator satisfies the recursion s(t)^2 = lambda*r(t-1)^2 + (1 - lambda)*s(t-1)^2
#' where r(t) is the (continuously compounded) return and lambda is the exponential decay parameter.
#' The recursion is typically initiated with the sample variance for returns.
#' 
#' @return An object of class \code{"ewmaVol"} for which there are \code{print}, \code{plot}, 
#' and \code{predict} methods, and extractor functions \code{fitted} and \code{residuals}.
#' 
#' An object of class \code{ewmaVol} is a list with the following components
#' \itemize{
#'  \item \code{returns}. \code{xts} vector of returns.
#'  \item \code{sigma}. \code{xts} vector of EWMA volatility estimates.
#'  \item \code{overlap}. Scalar overlap value.
#'  \item \code{call}. Function call.
#'  \item \code{lambda}. Scalar exponential decay value.
#'  \item \code{half.life}. Scalar half-life value.
#' }
#' 
#' @examples
#' \dontrun{
#' ewmavol(r)
#' }
ewmaVol <- function(r, lambda=NULL, half.life=NULL, overlap=1, 
                    demean=c("none", "sample", "ewma")) {
  require(PerformanceAnalytics)
  call = match.call()
  r = checkData(r, method="xts")
  if (ncol(r) > 1) {
    stop("r must be univariate")
  }

  # check for lambda value
  if (!is.null(lambda)) {
    if (lambda > 1 || lambda == 0){
      stop("lambda must be between 0 and 1.")
    } else {
      half.life = log(0.5)/log(lambda)
    }
  } else {
    # check for half.life
    if (!is.null(half.life)) {
      if (half.life <= 0) {
        stop("Half-life must be positive")
      } else {
        lambda = exp(log(0.5)/half.life)
      }
    } else {
      stop("Must specify either lambda or half.life")
    }
  } 
  
  ewmaVar.mat <- matrix(0, length(r), 1)
  r.mat = coredata(r)
  n.obs <- length(r)
  ## recursive formula: s2(t) = (1-lambda)*r2(t-1) + lambda*s2(t-1), initialize at sample variance
  ewmaVar.mat[1] <- as.numeric(var(r))
  for (i in 2:n.obs) {
    ewmaVar.mat[i] = lambda*ewmaVar.mat[i-1]+ (1 - lambda)*r.mat[i-1]^2
  }
  ewmaVol.xts <- xts(sqrt(ewmaVar.mat), index(r))  
  colnames(ewmaVol.xts) = colnames(r)
  ans = list(returns = r,
             sigma = ewmaVol.xts,
             call = call,
             overlap = overlap,
             lambda = lambda,
             half.life = half.life)
  class(ans) = "ewmaVol"
  return(ans)
}
