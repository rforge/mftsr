\name{yhoo.df}
\alias{yhoo.df}
\docType{data}
\title{
daily high, low, open and close prices as well as volume information
for Yahoo stock for the month of February 2002
}
\description{
daily high, low, open and close prices as well as volume information
for Yahoo stock for the month of February 2002
}
\usage{data(yhoo.df)}
\format{
  A data frame with 19 observations on the following 6 variables.
  \describe{
    \item{\code{Date}}{a factor with levels \code{1-Feb-02} \code{11-Feb-02} \code{12-Feb-02} \code{13-Feb-02} \code{14-Feb-02} \code{15-Feb-02} \code{19-Feb-02} \code{20-Feb-02} \code{21-Feb-02} \code{22-Feb-02} \code{25-Feb-02} \code{26-Feb-02} \code{27-Feb-02} \code{28-Feb-02} \code{4-Feb-02} \code{5-Feb-02} \code{6-Feb-02} \code{7-Feb-02} \code{8-Feb-02}}
    \item{\code{Open}}{a numeric vector}
    \item{\code{High}}{a numeric vector}
    \item{\code{Low}}{a numeric vector}
    \item{\code{Close}}{a numeric vector}
    \item{\code{Volume}}{a numeric vector}
  }
}
\source{
S+ FinMetrics yhoo.df.
}
\references{
S+ FinMetrics yhoo.df.
}
\examples{
data(yhoo.df)
## maybe str(yhoo.df) ; plot(yhoo.df) ...
}
\keyword{datasets}
