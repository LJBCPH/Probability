\name{BTFunktioner}
\alias{BTFunktioner}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{BTFunktioner
%%  ~~function to do ... ~~
}
\description{
Laver likelihoodfunktion og differentialer
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
BTFunktioner(beta, theta, x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{beta}{
%%     ~~Describe \code{beta} here~~
}
  \item{theta}{
%%     ~~Describe \code{theta} here~~
}
  \item{x}{
%%     ~~Describe \code{x} here~~
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
Victor Emil Skov Lundmark
Lucas Johan Boesen
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
function (beta, theta, x)
{
    logl <- function(beta, theta, x) {
        sum = 0
        for (i in 1:(dim(x)[2] - 1)) {
            for (j in (i + 1):(dim(x)[2])) {
                sum = sum + Y[i, j] * (t(x[, i]) \%*\% beta - log(exp(t(x[,
                  i]) \%*\% beta) + theta * exp(t(x[, j]) \%*\% beta))) +
                  Y[j, i] * (t(x[, j]) \%*\% beta - log(exp(t(x[,
                    j]) \%*\% beta) + theta * exp(t(x[, i]) \%*\%
                    beta))) + (r[i, j] - Y[i, j] - Y[j, i]) *
                  (log(theta^2 - 1) + t(x[, i]) \%*\% beta + t(x[,
                    j]) \%*\% beta - log(exp(t(x[, i]) \%*\% beta) +
                    theta * exp(t(x[, j]) \%*\% beta)) - log(exp(t(x[,
                    j]) \%*\% beta) + theta * exp(t(x[, i]) \%*\%
                    beta)))
            }
        }
        return(sum)
    }
    db <- function(beta, theta, x) {
        sum = 0
        for (i in 1:(dim(x)[2] - 1)) {
            for (j in (i + 1):dim(x)[2]) {
                sum = sum + (c(r[i, j] - Y[i, j]) * (-(theta *
                  exp(t(x[, i]) \%*\% beta))/(exp(t(x[, j]) \%*\%
                  beta) + theta * exp(t(x[, i]) \%*\% beta))) +
                  c(r[i, j] - Y[j, i]) * ((theta * exp(t(x[,
                    j]) \%*\% beta))/(exp(t(x[, i]) \%*\% beta) +
                    theta * exp(t(x[, j]) \%*\% beta)))) * (x[,
                  i] - x[, j])
            }
        }
        return(sum)
    }
    dtheta <- function(beta, theta, x) {
        for (i in 1:(dim(x)[2] - 1)) {
            for (j in (i + 1):dim(x)[2]) {
                sum = sum + ((r[i, j] - Y[i, j] - Y[j, i]) *
                  (2 * theta/(theta^2 - 1)) + (r[i, j] - Y[i,
                  j]) * (-(exp(t(x[, i]) \%*\% beta)/(theta * exp(t(x[,
                  i]) \%*\% beta) + exp(t(x[, j]) \%*\% beta)))) +
                  (r[i, j] - Y[j, i]) * (-(exp(t(x[, j]) \%*\%
                    beta)/(exp(t(x[, i]) \%*\% beta) + theta *
                    exp(t(x[, j]) \%*\% beta)))))
            }
        }
        return(sum)
    }
    dtheta2 <- function(beta, theta, x) {
        for (i in 1:(dim(x)[2] - 1)) {
            for (j in (i + 1):dim(x)[2]) {
                sum = sum + ((r[i, j] - Y[i, j] - Y[j, i]) *
                  (-(2 * (theta^2 + 1))/(theta^2 - 1)^2) + (r[i,
                  j] - Y[i, j]) * (exp(2 * t(x[, i]) \%*\% beta)/(theta *
                  exp(t(x[, i]) \%*\% beta) + exp(t(x[, j]) \%*\%
                  beta))^2) + (r[i, j] - Y[j, i]) * (exp(2 *
                  t(x[, j]) \%*\% beta)/(exp(t(x[, i]) \%*\% beta) +
                  theta * exp(t(x[, j]) \%*\% beta))^2))
            }
        }
        return(sum)
    }
    db2 <- function(beta, theta, x) {
        for (i in 1:(dim(x)[2] - 1)) {
            for (j in (i + 1):dim(x)[2]) {
                sum = sum + (as.numeric((((-(r[i, j] - Y[i, j])/(theta *
                  exp(t(x[, i]) \%*\% beta) + exp(t(x[, j]) \%*\%
                  beta))^2) - (r[i, j] - Y[j, i])/(exp(t(x[,
                  i]) \%*\% beta) + theta * exp(t(x[, j]) \%*\% beta))^2)) *
                  (exp(t(x[, i]) \%*\% beta + t(x[, j]) \%*\% beta) *
                    theta)) * ((x[, i] - x[, j]) \%*\% t(x[, i] -
                  x[, j])))
            }
        }
        return(sum)
    }
    dbt <- function(beta, theta, x) {
        for (i in 1:(dim(x)[2] - 1)) {
            for (j in (i + 1):dim(x)[2]) {
                sum = sum + ((as.numeric((-(r[i, j] - Y[i, j])/(theta *
                  exp(t(x[, i]) \%*\% beta) + exp(t(x[, j]) \%*\%
                  beta))^2) + ((r[i, j] - Y[j, i])/(exp(t(x[,
                  i]) \%*\% beta) + theta * exp(t(x[, j]) \%*\% beta))^2)) *
                  (exp(t(x[, i]) \%*\% beta + t(x[, j]) \%*\% beta)) *
                  (x[, i] - x[, j])))
            }
        }
        return(sum)
    }
    Funktions <- list(loglike = logl, dlbeta = db, dltheta = dtheta,
        dl2xtheta = dtheta2, dl2xbeta = db2, dlbetatheta = dbt)
    return(Funktions)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
