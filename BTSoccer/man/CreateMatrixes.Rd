\name{CreateMatrixes}
\alias{CreateMatrixes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{CreateMatrixes
%%  ~~function to do ... ~~
}
\description{
Laver Design, Kontingenstabel og samledekampe
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CreateMatrixes(data, StartDate, EndDate, round)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{StartDate}{
%%     ~~Describe \code{StartDate} here~~
}
  \item{EndDate}{
%%     ~~Describe \code{EndDate} here~~
}
  \item{round}{
%%     ~~Describe \code{round} here~~
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
function (data, StartDate, EndDate, round)
{
    data$H <- as.character(data$H)
    data$U <- as.character(data$U)
    data$dato <- as.Date(data$dato, format = "\%m/\%d/\%Y")
    data1 <- data[which((data$dato >= StartDate) & (data$dato <=
        EndDate)), ]
    dataUNan <- na.omit(data1)
    data1$HU <- paste(data1$H, data1$U)
    UnikSammenligner <- unique(data1$HU)
    hjemmesejre <- aggregate(data1$Hsejr, by = list(HU = data1$HU),
        FUN = sum)
    hjemmesejre <- hjemmesejre[order(hjemmesejre$HU), ]
    udesejre <- aggregate(data1$Usejr, by = list(HU = data1$HU),
        FUN = sum)
    udesejre <- udesejre[order(udesejre$HU), ]
    udesejre <- separate(data = udesejre, col = HU, into = c("H1",
        "H2"), sep = " ")
    udesejre$HU <- paste(udesejre$H2, udesejre$H1)
    udesejre <- udesejre[, 4:3]
    udesejre <- udesejre[order(udesejre$HU), ]
    HUsejre <- c(hjemmesejre$x + udesejre$x)
    samlet <- cbind.data.frame(hjemmesejre$HU, HUsejre)
    colnames(samlet)[1] <- "Hold"
    samlet <- separate(data = samlet, col = Hold, into = c("H1",
        "H2"), sep = " ")
    Y <- xtabs(samlet$HUsejre ~ H1 + H2, samlet)
    Y <- as.data.frame.matrix(Y)
    UnikHold <- unique(data1$H)
    UnikHold <- sort(UnikHold)
    streak <- c(rep(1, length(UnikHold)))
    for (hold in 1:length(UnikHold)) {
        StreakSum = 0
        for (kamp in 1:length(data1[, 1])) {
            if (data1$H[kamp] == UnikHold[hold]) {
                if (data1$HM[kamp] >= data1$UM[kamp]) {
                  StreakSum = StreakSum + 1
                }
                else {
                  StreakSum = 0
                }
            }
            else if (data1$U[kamp] == UnikHold[hold]) {
                if (data1$HM[kamp] <= data1$UM[kamp]) {
                  StreakSum = StreakSum + 1
                }
                else {
                  StreakSum = 0
                }
            }
        }
        streak[hold] = StreakSum
    }
    GnsMal <- c(aggregate(data1$HM, by = list(H = data1$H), FUN = mean)[,
        2] + aggregate(data1$UM, by = list(U = data1$U), FUN = mean)[,
        2])/2
    GnsMalInd <- c(aggregate(data1$UM, by = list(H = data1$H),
        FUN = mean)[, 2] + aggregate(data1$HM, by = list(U = data1$U),
        FUN = mean)[, 2])/2
    GnsTilskuer <- c(aggregate(data1$Tilskuere, by = list(H = data1$H),
        FUN = mean)[, 2] + aggregate(data1$Tilskuere, by = list(U = data1$U),
        FUN = mean)[, 2])/1000
    GnsBoldBes <- c(aggregate(dataUNan$boldb_h, by = list(H = dataUNan$H),
        FUN = mean)[, 2] + aggregate(dataUNan$boldb_u, by = list(U = dataUNan$U),
        FUN = mean)[, 2])
    GnsSkud <- c(aggregate(dataUNan$skud_h, by = list(H = dataUNan$H),
        FUN = mean)[, 2] + aggregate(dataUNan$skud_u, by = list(U = dataUNan$U),
        FUN = mean)[, 2])/2
    GnsSkudIndenfor <- c(aggregate(dataUNan$skud_inderfor_h,
        by = list(H = dataUNan$H), FUN = mean)[, 2] + aggregate(dataUNan$skud_inderfor_u,
        by = list(U = dataUNan$U), FUN = mean)[, 2])/2
    GnsFrispark <- c(aggregate(dataUNan$frispark_h, by = list(H = dataUNan$H),
        FUN = mean)[, 2] + aggregate(dataUNan$frispark_u, by = list(U = dataUNan$U),
        FUN = mean)[, 2])/2
    GnsHjorne <- c(aggregate(dataUNan$hjorne_h, by = list(H = dataUNan$H),
        FUN = mean)[, 2] + aggregate(dataUNan$hjorne_u, by = list(U = dataUNan$U),
        FUN = mean)[, 2])/2
    x <- as.data.frame.matrix(rbind(streak, GnsHjorne, GnsMal,
        GnsMalInd, GnsTilskuer, GnsBoldBes, GnsSkud, GnsSkudIndenfor,
        GnsFrispark))
    r <- xtabs(data1$Hsejr + data1$Usejr + data1$Uafgjort ~ H +
        U, data1)
    r <- as.data.frame.matrix(r)
    r <- r + t(r)
    Matrixes <- list(DesignMatrix = x, KontingensTabel = Y, SamledeKampe = r)
    return(Matrixes)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
