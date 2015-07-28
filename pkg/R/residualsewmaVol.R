#' @title residuals.ewmavol()
#' 
#' @author Eric Zivot
#' 
#' @description
#' residuals.ewmavol()
#' 
#' @param object object
#' @param standardize standardize=FALSE
#' 
#' @return
#' object$returns/object$sigma
#' 
#' @examples
#' \dontrun{
#' residuals.ewmavol(r)
#' }
#' 
#' @export residuals.ewmaVol
residuals.ewmaVol <- function(object, standardize=FALSE) {
  if (!standardize) {
    return(object$returns)
  } else {
    return(object$returns/object$sigma)
  }
}