\name{NR}
\alias{NR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{NR
%%  ~~function to do ... ~~
}
\description{
Laver Newton-Raphson approksimation
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
NR(x, f)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{f}{
%%     ~~Describe \code{f} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, f)
{
    ite = as.matrix(c(rep(0.1, dim(x)[1]), 1.1))
    counter = 0
    val = 1
    while (abs(val) > 1e-07) {
        beta = c(ite[1:(dim(x)[1])])
        theta = ite[dim(x)[1] + 1]
        a12 = as.matrix(f$dlbeta(beta, theta, x))
        grad = rbind(a12, f$dltheta(beta, theta, x))
        A = f$dl2xbeta(beta, theta, x)
        B = as.matrix(f$dlbetatheta(beta, theta, x))
        C = t(as.matrix(f$dlbetatheta(beta, theta, x)))
        D = f$dl2xtheta(beta, theta, x)
        inf = cbind(A, B)
        inf = rbind(inf, c(C, D))
        inf = -inf
        temp = ite
        ite = ite + Inverse(inf) \%*\% grad * (1/2)
        val = sum(temp - ite)
        f$loglike(beta, theta, x)
        counter = counter + 1
    }
    cat("logl :", f$loglike(beta, theta, x))
    styrker <- exp(t(x) \%*\% beta)
    KV <- inv(inf)
    U <- sqrt(diag(KV))
    Values = list(beta = beta, theta = theta, Styrker = styrker,
        sd = U)
    return(Values)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
