\name{mk.zero2}
\alias{mk.zero2}
\alias{mk.fwd2}
\alias{mk.maturity}
\docType{data}
\title{
U.S. zero coupon rates and forward rates for January 1990
}
\description{
U.S. zero coupon rates and forward rates, respectively, computed by McCulloch and Kwon (1993). Both mk.zero2 and mk.fwd2 are dataframe objects with 56 columns, first column is the position, the rest each column representing the rate with the corresponding maturity in the 55 × 1 vector mk.maturity.For example, the first element of the vector mk.maturity is 0.083, so the first columns of mk.zero2 and mk.fwd2 correspond to the rates with maturity of one month.
}
\usage{data(mk.zero2)}
\format{
  A data frame with 67 observations on the following 56 variables.
  \describe{
    \item{\code{RowNames}}{a factor with levels \code{Apr 1986} \code{Apr 1987} \code{Apr 1988} \code{Apr 1989} \code{Apr 1990} \code{Aug 1985} \code{Aug 1986} \code{Aug 1987} \code{Aug 1988} \code{Aug 1989} \code{Aug 1990} \code{Dec 1985} \code{Dec 1986} \code{Dec 1987} \code{Dec 1988} \code{Dec 1989} \code{Dec 1990} \code{Feb 1986} \code{Feb 1987} \code{Feb 1988} \code{Feb 1989} \code{Feb 1990} \code{Feb 1991} \code{Jan 1986} \code{Jan 1987} \code{Jan 1988} \code{Jan 1989} \code{Jan 1990} \code{Jan 1991} \code{Jul 1986} \code{Jul 1987} \code{Jul 1988} \code{Jul 1989} \code{Jul 1990} \code{Jun 1986} \code{Jun 1987} \code{Jun 1988} \code{Jun 1989} \code{Jun 1990} \code{Mar 1986} \code{Mar 1987} \code{Mar 1988} \code{Mar 1989} \code{Mar 1990} \code{May 1986} \code{May 1987} \code{May 1988} \code{May 1989} \code{May 1990} \code{Nov 1985} \code{Nov 1986} \code{Nov 1987} \code{Nov 1988} \code{Nov 1989} \code{Nov 1990} \code{Oct 1985} \code{Oct 1986} \code{Oct 1987} \code{Oct 1988} \code{Oct 1989} \code{Oct 1990} \code{Sep 1985} \code{Sep 1986} \code{Sep 1987} \code{Sep 1988} \code{Sep 1989} \code{Sep 1990}}
    \item{\code{M.1}}{a numeric vector}
    \item{\code{M.2}}{a numeric vector}
    \item{\code{M.3}}{a numeric vector}
    \item{\code{M.4}}{a numeric vector}
    \item{\code{M.5}}{a numeric vector}
    \item{\code{M.6}}{a numeric vector}
    \item{\code{M.7}}{a numeric vector}
    \item{\code{M.8}}{a numeric vector}
    \item{\code{M.9}}{a numeric vector}
    \item{\code{M.10}}{a numeric vector}
    \item{\code{M.11}}{a numeric vector}
    \item{\code{M.12}}{a numeric vector}
    \item{\code{M.13}}{a numeric vector}
    \item{\code{M.14}}{a numeric vector}
    \item{\code{M.15}}{a numeric vector}
    \item{\code{M.16}}{a numeric vector}
    \item{\code{M.17}}{a numeric vector}
    \item{\code{M.18}}{a numeric vector}
    \item{\code{M.19}}{a numeric vector}
    \item{\code{M.20}}{a numeric vector}
    \item{\code{M.21}}{a numeric vector}
    \item{\code{M.22}}{a numeric vector}
    \item{\code{M.23}}{a numeric vector}
    \item{\code{M.24}}{a numeric vector}
    \item{\code{M.25}}{a numeric vector}
    \item{\code{M.26}}{a numeric vector}
    \item{\code{M.27}}{a numeric vector}
    \item{\code{M.28}}{a numeric vector}
    \item{\code{M.29}}{a numeric vector}
    \item{\code{M.30}}{a numeric vector}
    \item{\code{M.31}}{a numeric vector}
    \item{\code{M.32}}{a numeric vector}
    \item{\code{M.33}}{a numeric vector}
    \item{\code{M.34}}{a numeric vector}
    \item{\code{M.35}}{a numeric vector}
    \item{\code{M.36}}{a numeric vector}
    \item{\code{M.37}}{a numeric vector}
    \item{\code{M.38}}{a numeric vector}
    \item{\code{M.39}}{a numeric vector}
    \item{\code{M.40}}{a numeric vector}
    \item{\code{M.41}}{a numeric vector}
    \item{\code{M.42}}{a numeric vector}
    \item{\code{M.43}}{a numeric vector}
    \item{\code{M.44}}{a numeric vector}
    \item{\code{M.45}}{a numeric vector}
    \item{\code{M.46}}{a numeric vector}
    \item{\code{M.47}}{a numeric vector}
    \item{\code{M.48}}{a numeric vector}
    \item{\code{M.49}}{a logical vector}
    \item{\code{M.50}}{a logical vector}
    \item{\code{M.51}}{a logical vector}
    \item{\code{M.52}}{a logical vector}
    \item{\code{M.53}}{a logical vector}
    \item{\code{M.54}}{a logical vector}
    \item{\code{M.55}}{a logical vector}
  }
}

\source{
S+ FinMetrics "mk.maturity", "mk.zero2" and "mk.fwd2"
}
\references{
McCulloch and Kwon (1993).
}
\examples{
data(mk.zero2)
## maybe str(mk.zero2) ; plot(mk.zero2) ...
}
\keyword{datasets}
