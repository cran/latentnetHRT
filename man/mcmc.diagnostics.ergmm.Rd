\name{mcmc.diagnostics.ergmm}
\alias{mcmc.diagnostics.ergmm}
\alias{mcmc.diagnostics}
\alias{ergmm.raftery.diag}
\title{Conduct MCMC diagnostics on an ergmm fit}
\description{
  This function creates simple diagnostic plots for the MCMC sampled
  statistics produced from a fit. It also prints the Raftery-Lewis 
  diagnostics, indicates if they are sufficient,
  and suggests the run length required.
}
\usage{
\method{mcmc.diagnostics}{ergmm}(object, sample = "sample",
                      smooth=TRUE, r = 0.0125, digits = 6,
                      maxplot = 1000, verbose = TRUE, 
                      mcmc.title = "Summary of MCMC samples", \dots)
}
\arguments{
  \item{object}{An object.  See documentation for \code{\link{ergmm}}.}
  \item{sample}{The component of \code{object} on which the
    diagnosis is based. This is 
    usually sample of the trace statistics from the model.}
  \item{smooth}{Draw a smooth line through trace plots}
  \item{r}{Percentile of the distribution to estimate}
  \item{digits}{Number of digits to print}
  \item{maxplot}{Maximum number of statistics to plot}
  \item{verbose}{If this is \code{TRUE}, print out more information about the MCMC runs including lag correlations.}
  \item{mcmc.title}{Figure title for the diagnostic plots.}
  \item{\dots}{Additional arguments, to be passed to lower-level functions
  in the future.}
}

\details{
The plots produced are a trace of the sampled output
and a density estimate for each variable in the chain.

The Raftery-Lewis diagnostic is a run length control diagnostic based on a
criterion of accuracy of estimation of the quantile q. It
is intended for use on a short pilot run of a Markov chain.
The number of iterations required to estimate the quantile
q to within an accuracy of +/- r with probability p is
calculated. Separate calculations are performed for each
variable within each chain.

In fact, an \code{object} contains the matrix of
statistics from the MCMC run as component \code{$sample}.
This matrix is actually an object of class \code{mcmc} and
can be used directly in the \code{CODA} package to assess MCMC
convergence. \emph{Hence all MCMC diagnostic methods available
in \code{\link[coda]{coda}} are available directly.} See the examples and
\url{http://www.mrc-bsu.cam.ac.uk/bugs/classic/coda04/readme.shtml}.

More information can be found by looking at the documentation of
\code{\link{ergmm}}.
  
% \code{mcgibbsit} computes the minimum run length \eqn{N_{min}}{Nmin},
% required burn in \eqn{M}, total run length \eqn{N}, run length
% inflation due to \emph{auto-correlation}, \eqn{I}, and the run length
% inflation due to \emph{between-chain} correlation, \eqn{R} for a set of
% exchangeable MCMC simulations which need not be independent.
}
\references{
  Warnes, G.W. (2000).  Multi-Chain and Parallel Algorithms for Markov
  Chain Monte Carlo. Dissertation, Department of Biostatistics,
  University of Washington,

  Raftery, A.E. and Lewis, S.M. (1992).  One long run with diagnostics:
  Implementation strategies for Markov chain Monte Carlo. Statistical
  Science, 7, 493-497.

  Raftery, A.E. and Lewis, S.M. (1995).  The number of iterations,
  convergence diagnostics and generic Metropolis algorithms.  In
  Practical Markov Chain Monte Carlo (W.R. Gilks, D.J. Spiegelhalter and
  S. Richardson, eds.). London, U.K.: Chapman and Hall.
}
\author{Mark S. Handcock, \email{handcock@stat.washington.edu}
  based on the \code{\link[coda]{coda}} package and also ideas
  from \code{\link[mcgibbsit]{mcgibbsit}} by 
  Gregory R. Warnes \email{gregory\_r\_warnes@groton.pfizer.com}.
  It is based on the the R function 
  \code{raftery.diag} in \code{\link[coda]{coda}}.
  \code{raftery.diag}, in turn, is based on the
  FORTRAN program \code{gibbsit} written by Steven Lewis which is available
  from the Statlib archive.  }

\value{\code{\link{mcmc.diagnostics.ergmm}} returns a matrix of 
       Raftery-Lewis diagnostics.
}
\section{Details of output}{
    \describe{
      \item{M}{The number of \code{burn in} iterations to be discarded
        (total over all chains).}
      \item{N}{The number of iterations after burn in required to
        estimate the quantile q to within an accuracy of
        +/- r with probability p (total over all chains).
        The overall number of iterations required (M + N).}
      \item{Total}{Overall number of iterations required (M + N).}
      \item{Nmin}{The minimum required sample size for a chain with no 
        correlation between consecutive samples. Positive 
        autocorrelation will increase the required sample
        size above this minimum value.}
       \item{I}{An estimate (the \code{dependence factor}) of the extent to
         which auto-correlation inflates the required sample
         size.  Values of \code{I} larger than 5 indicate strong
         autocorrelation which may be due to a poor choice of
         starting value, high posterior correlations, or
         \code{stickiness} of the MCMC algorithm.}
    }
%     \item{R}{An estimate of the extent to which between-chain
%        correlation inflates the required sample size.  Large
%        values of \code{R} indicate that there is significant
%        correlation between the chains and may be indicative
%        of a lack of convergence or a poor multi-chain
%        algorithm.}
}
\seealso{\code{\link{ergmm}}, \code{\link[network]{network}},
         \code{\link[coda]{coda}}, \code{\link[mcgibbsit]{mcgibbsit}}, 
         \code{\link{summary.ergmm}}
        }
\examples{
#
data(sampson)
#
# test the mcmc.diagnostics function
#
gest <- ergmm(samplike ~ latent(k=2))
summary(gest)

#
# Plot the traces and densities
#
mcmc.diagnostics(gest)
#
# Use coda directly
#
library(coda)
#
plot(gest$sample, ask=FALSE)
#
# A full range of diagnostics are available 
# using codamenu()
#
}
\keyword{models}
