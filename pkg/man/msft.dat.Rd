\name{msft.dat}
\alias{msft.dat}
\docType{data}
\title{
daily open, high, low and close prices and volume
on Microsoft stock from October 2, 2000 through August 31, 2001
}
\description{
daily open, high, low and close prices and volume
on Microsoft stock from October 2, 2000 through August 31, 2001
}
\usage{data(msft.dat)}
\format{
  A data frame with 249 observations on the following 6 variables.
  \describe{
    \item{\code{RowNames}}{a factor with levels \code{01/02/2001} \code{01/03/2001} \code{01/04/2001} \code{01/05/2001} \code{01/08/2001} \code{01/09/2001} \code{01/10/2001} \code{01/11/2001} \code{01/12/2001} \code{01/16/2001} \code{01/17/2001} \code{01/18/2001} \code{01/19/2001} \code{01/22/2001} \code{01/23/2001} \code{01/24/2001} \code{01/25/2001} \code{01/26/2001} \code{01/29/2001} \code{01/30/2001} \code{01/31/2001} \code{02/01/2001} \code{02/02/2001} \code{02/05/2001} \code{02/06/2001} \code{02/07/2001} \code{02/08/2001} \code{02/09/2001} \code{02/12/2001} \code{02/13/2001} \code{02/14/2001} \code{02/15/2001} \code{02/16/2001} \code{02/20/2001} \code{02/21/2001} \code{02/22/2001} \code{02/23/2001} \code{02/26/2001} \code{02/27/2001} \code{02/28/2001} \code{03/01/2001} \code{03/02/2001} \code{03/05/2001} \code{03/06/2001} \code{03/07/2001} \code{03/08/2001} \code{03/09/2001} \code{03/12/2001} \code{03/13/2001} \code{03/14/2001} \code{03/15/2001} \code{03/16/2001} \code{03/19/2001} \code{03/20/2001} \code{03/21/2001} \code{03/22/2001} \code{03/23/2001} \code{03/26/2001} \code{03/27/2001} \code{03/28/2001} \code{03/29/2001} \code{03/30/2001} \code{04/02/2001} \code{04/03/2001} \code{04/04/2001} \code{04/05/2001} \code{04/06/2001} \code{04/09/2001} \code{04/10/2001} \code{04/11/2001} \code{04/12/2001} \code{04/16/2001} \code{04/17/2001} \code{04/18/2001} \code{04/19/2001} \code{04/20/2001} \code{04/23/2001} \code{04/24/2001} \code{04/25/2001} \code{04/26/2001} \code{04/27/2001} \code{04/30/2001} \code{05/01/2001} \code{05/02/2001} \code{05/03/2001} \code{05/04/2001} \code{05/07/2001} \code{05/08/2001} \code{05/09/2001} \code{05/10/2001} \code{05/11/2001} \code{05/14/2001} \code{05/15/2001} \code{05/16/2001} \code{05/17/2001} \code{05/18/2001} \code{05/21/2001} \code{05/22/2001} \code{05/23/2001} \code{05/24/2001} \code{05/25/2001} \code{05/29/2001} \code{05/30/2001} \code{05/31/2001} \code{06/01/2001} \code{06/04/2001} \code{06/05/2001} \code{06/06/2001} \code{06/07/2001} \code{06/08/2001} \code{06/11/2001} \code{06/12/2001} \code{06/13/2001} \code{06/14/2001} \code{06/15/2001} \code{06/18/2001} \code{06/19/2001} \code{06/20/2001} \code{06/21/2001} \code{06/22/2001} \code{06/25/2001} \code{06/26/2001} \code{06/27/2001} \code{06/28/2001} \code{06/29/2001} \code{07/02/2001} \code{07/03/2001} \code{07/05/2001} \code{07/06/2001} \code{07/09/2001} \code{07/10/2001} \code{07/11/2001} \code{07/12/2001} \code{07/13/2001} \code{07/16/2001} \code{07/17/2001} \code{07/18/2001} \code{07/19/2001} \code{07/20/2001} \code{07/23/2001} \code{07/24/2001} \code{07/25/2001} \code{07/26/2001} \code{07/27/2001} \code{07/30/2001} \code{07/31/2001} \code{08/01/2001} \code{08/02/2001} \code{08/03/2001} \code{08/06/2001} \code{08/07/2001} \code{08/08/2001} \code{08/09/2001} \code{08/10/2001} \code{08/13/2001} \code{08/14/2001} \code{08/15/2001} \code{08/16/2001} \code{08/17/2001} \code{08/20/2001} \code{08/21/2001} \code{08/22/2001} \code{08/23/2001} \code{08/24/2001} \code{08/27/2001} \code{08/28/2001} \code{08/29/2001} \code{08/30/2001} \code{08/31/2001} \code{09/04/2001} \code{09/05/2001} \code{09/06/2001} \code{09/07/2001} \code{09/10/2001} \code{09/17/2001} \code{09/18/2001} \code{09/19/2001} \code{09/20/2001} \code{09/21/2001} \code{09/24/2001} \code{09/25/2001} \code{09/26/2001} \code{09/27/2000} \code{09/27/2001} \code{09/28/2000} \code{09/29/2000} \code{10/02/2000} \code{10/03/2000} \code{10/04/2000} \code{10/05/2000} \code{10/06/2000} \code{10/09/2000} \code{10/10/2000} \code{10/11/2000} \code{10/12/2000} \code{10/13/2000} \code{10/16/2000} \code{10/17/2000} \code{10/18/2000} \code{10/19/2000} \code{10/20/2000} \code{10/23/2000} \code{10/24/2000} \code{10/25/2000} \code{10/26/2000} \code{10/27/2000} \code{10/30/2000} \code{10/31/2000} \code{11/01/2000} \code{11/02/2000} \code{11/03/2000} \code{11/06/2000} \code{11/07/2000} \code{11/08/2000} \code{11/09/2000} \code{11/10/2000} \code{11/13/2000} \code{11/14/2000} \code{11/15/2000} \code{11/16/2000} \code{11/17/2000} \code{11/20/2000} \code{11/21/2000} \code{11/22/2000} \code{11/24/2000} \code{11/27/2000} \code{11/28/2000} \code{11/29/2000} \code{11/30/2000} \code{12/01/2000} \code{12/04/2000} \code{12/05/2000} \code{12/06/2000} \code{12/07/2000} \code{12/08/2000} \code{12/11/2000} \code{12/12/2000} \code{12/13/2000} \code{12/14/2000} \code{12/15/2000} \code{12/18/2000} \code{12/19/2000} \code{12/20/2000} \code{12/21/2000} \code{12/22/2000} \code{12/26/2000} \code{12/27/2000} \code{12/28/2000} \code{12/29/2000}}
    \item{\code{Open}}{a numeric vector}
    \item{\code{High}}{a numeric vector}
    \item{\code{Low}}{a numeric vector}
    \item{\code{Close}}{a numeric vector}
    \item{\code{Volume}}{a numeric vector}
  }
}
\source{
S+ FinMetrics msft.dat.
}
\references{
S+ FinMetrics msft.dat.
}
\examples{
data(msft.dat)
## maybe str(msft.dat) ; plot(msft.dat) ...
}
\keyword{datasets}
