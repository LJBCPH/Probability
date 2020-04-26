\name{Sandsynligheder}
\alias{Sandsynligheder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Sandsynligheder
%%  ~~function to do ... ~~
}
\description{
Udregner ssh. for at hold i vinder over hold j, ved ssh'erne: (i sejr, j sejr, uafgjort)
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Sandsynligheder(beta, theta, x, i, j)
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
  \item{i}{
%%     ~~Describe \code{i} here~~
}
  \item{j}{
%%     ~~Describe \code{j} here~~
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
function (beta, theta, x, i, j)
{
    VTU = 0
    VTU = c((styrker[i])/(styrker[i] + theta * styrker[j]), ((styrker[j])/(styrker[i] *
        theta + styrker[j])), ((styrker[i] * styrker[j] * (theta^2 -
        1))/((styrker[i] + theta * styrker[j]) * (styrker[i] *
        theta + styrker[j]))))
    return(VTU)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
