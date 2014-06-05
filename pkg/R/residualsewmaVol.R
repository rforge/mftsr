# residuals.ewmaVol.R

residuals.ewmaVol <- function(object, standardize=FALSE) {
  if (!standardize) {
    return(object$returns)
  } else {
    return(object$returns/object$sigma)
  }
}