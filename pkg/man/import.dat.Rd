\name{import.dat}
\alias{import.dat}
\alias{newtaxes.dat}
\docType{data}
\title{
Argentinian import data
}
\description{
import.dat contains two monthly time series from January 1983 to December 1990. The first series "taxes"" corresponds to Argentinian import taxes and the second "import"" to Argentinian imports.

newtaxes.dat contains the values of the variable "taxes"
from January 1992 to October 1992.
}
\usage{data(import.dat)}
\format{
  A data frame with 96 observations on the following 3 variables.
  \describe{
    \item{\code{RowNames}}{a factor with levels \code{Apr 1983} \code{Apr 1984} \code{Apr 1985} \code{Apr 1986} \code{Apr 1987} \code{Apr 1988} \code{Apr 1989} \code{Apr 1990} \code{Aug 1983} \code{Aug 1984} \code{Aug 1985} \code{Aug 1986} \code{Aug 1987} \code{Aug 1988} \code{Aug 1989} \code{Aug 1990} \code{Dec 1983} \code{Dec 1984} \code{Dec 1985} \code{Dec 1986} \code{Dec 1987} \code{Dec 1988} \code{Dec 1989} \code{Dec 1990} \code{Feb 1983} \code{Feb 1984} \code{Feb 1985} \code{Feb 1986} \code{Feb 1987} \code{Feb 1988} \code{Feb 1989} \code{Feb 1990} \code{Jan 1983} \code{Jan 1984} \code{Jan 1985} \code{Jan 1986} \code{Jan 1987} \code{Jan 1988} \code{Jan 1989} \code{Jan 1990} \code{Jul 1983} \code{Jul 1984} \code{Jul 1985} \code{Jul 1986} \code{Jul 1987} \code{Jul 1988} \code{Jul 1989} \code{Jul 1990} \code{Jun 1983} \code{Jun 1984} \code{Jun 1985} \code{Jun 1986} \code{Jun 1987} \code{Jun 1988} \code{Jun 1989} \code{Jun 1990} \code{Mar 1983} \code{Mar 1984} \code{Mar 1985} \code{Mar 1986} \code{Mar 1987} \code{Mar 1988} \code{Mar 1989} \code{Mar 1990} \code{May 1983} \code{May 1984} \code{May 1985} \code{May 1986} \code{May 1987} \code{May 1988} \code{May 1989} \code{May 1990} \code{Nov 1983} \code{Nov 1984} \code{Nov 1985} \code{Nov 1986} \code{Nov 1987} \code{Nov 1988} \code{Nov 1989} \code{Nov 1990} \code{Oct 1983} \code{Oct 1984} \code{Oct 1985} \code{Oct 1986} \code{Oct 1987} \code{Oct 1988} \code{Oct 1989} \code{Oct 1990} \code{Sep 1983} \code{Sep 1984} \code{Sep 1985} \code{Sep 1986} \code{Sep 1987} \code{Sep 1988} \code{Sep 1989} \code{Sep 1990}}
    \item{\code{taxes}}{a numeric vector}
    \item{\code{import}}{a numeric vector}
  }
}
\source{
S+ FinMetrics "import.dat".
}
\references{
S+ FinMetrics "import.dat".
}
\examples{
data(import.dat)
## maybe str(import.dat) ; plot(import.dat) ...
}
\keyword{datasets}
