#' @title print.ewmavol()
#' 
#' @author Eric Zivot
#' 
#' @description
#' print.ewmavol()
#' 
#' @param object object
#' @param ... additional arguments passed to \samp{print()}
#' 
#' @return
#' invisible(object)
#' 
#' @examples
#' \dontrun{
#' print.ewmavol(r)
#' }
#' 
#' @export print.ewmaVol
print.ewmaVol <- function(object, ...) {
  cat("EWMA volatility estimation:\n")
  cat("Call:\n")
  print(object$call)
  cat("lambda:  ", round(object$lambda, digits=2), "\n")
  cat("half-life:  ", round(object$half.life, digits=2), "\n")
  cat("current Vol estimate:  \n")
  print(last(object$sigma, n=1, ), ...)
  return(invisible(object))
} 
