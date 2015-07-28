#' @title fitted.ewmavol()
#' 
#' @author Eric Zivot
#' 
#' @description
#' fitted.ewmavol()
#' 
#' @param object object
#' 
#' @return
#' object$sigma
#' 
#' @examples
#' \dontrun{
#' fitted.ewmavol(r)
#' }
#' 
#' @export fitted.ewmaVol
fitted.ewmaVol <- function(object) {
  return(object$sigma)
}