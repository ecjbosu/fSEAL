\name{CDaR}
\alias{CDaR}
\alias{CDD}
\title{Calculate Uryasev's proposed Conditional Drawdown at Risk (CDD or CDaR)
measure}
\usage{
  CDaR(R, weights = NULL, geometric = TRUE, invert = TRUE,
    p = 0.95, ...)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{weights}{portfolio weighting vector, default NULL,
  see Details}

  \item{geometric}{utilize geometric chaining (TRUE) or
  simple/arithmetic chaining (FALSE) to aggregate returns,
  default TRUE}

  \item{invert}{TRUE/FALSE whether to invert the drawdown
  measure.  see Details.}

  \item{p}{confidence level for calculation, default
  p=0.95}

  \item{\dots}{any other passthru parameters}
}
\description{
  For some confidence level \eqn{p}, the conditional
  drawdown is the the mean of the worst \eqn{p\%}
  drawdowns.
}
\examples{
library(lpSolve)
data(edhec)
t(round(CDaR(edhec),4))
}
\author{
  Brian G. Peterson
}
\references{
  Chekhlov, A., Uryasev, S., and M. Zabarankin. Portfolio
  Optimization With Drawdown Constraints. B. Scherer (Ed.)
  Asset and Liability Management Tools, Risk Books, London,
  2003 http://www.ise.ufl.edu/uryasev/drawdown.pdf
}
\seealso{
  \code{\link{ES}} \code{\link{maxDrawdown}}
  \code{\link{CdarMultiPath}} \code{\link{AlphaDrawdown}}
  \code{\link{MultiBetaDrawdown}}
  \code{\link{BetaDrawdown}}
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

