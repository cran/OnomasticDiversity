\encoding{latin1}
\name{fSheldon}
\alias{fSheldon}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculate the Sheldon's diversity index }
\description{
   This function obtains the Sheldon's diversity index introduced by A. L. Sheldon. It is a method for quantifying species biodiversity that can be adapted to the context of onomastics.
}
\usage{
fSheldon (x, k, n, location, s)
}
\arguments{
  \item{x}{ dataframe of the data values for each species not null (because if you have a sample, there might be species that are not represented)..}
  \item{k}{ name of a variable which represents absolute frequency for each species.}
  \item{n}{ name of a variable which represents total number of individuals.}
  \item{location}{ represents the grouping element.}
  \item{s}{ vector which represents number of species.}
}
\details{
  For a community \eqn{i}{}, the Sheldon's diversity index is defined by
\eqn{E_{She} = \frac{2^{H^{\prime}}}{S_i}}{}, where \eqn{H^{\prime}}{} denotes the Shannon-Wiener index and \eqn{S_i}{} represents the number of species (richness).

In onomastic context, \eqn{S_i}{} are all surnames in region (\eqn{\approx}{} community diversity context) \eqn{i}{}.
}
\value{
  A dataframe containing the following components:
    \item{\code{location}}{ represents the grouping element, for example the communities / regions.}
    \item{\code{sheldon}}{ the value of the Pielou's diversity index.}
}
\references{
Sheldon, A. L. (1969). Equitability indices: dependence on the species count. \emph{Ecology}, \bold{50}, 466--467.
}

\seealso{
\code{\link{fMargalef}},
\code{\link{fMenhinick}},
\code{\link{fPielou}},
\code{\link{fShannon}},
\code{\link{fSimpson}},
\code{\link{fSimpsonInf}},
\code{\link{fGeneralisedMean}}, \code{\link{fGeometricMean}},
\code{\link{fHeip}}.}

\author{Maria Jose Ginzo Villamayor}

\examples{
library(sqldf)
data(surnamesgal14)
apes2=sqldf('select  muni, count(surname) as ni,
sum(number) as population from surnamesgal14
group by muni;')

result = fSheldon (x= surnamesgal14[surnamesgal14$number != 0,],
k="number", n="population", location  = "muni",
s = apes2$ni)
result

data(namesmengal16)
names2=sqldf('select  muni, count(name) as ni,
sum(number) as population from namesmengal16
group by muni;')

result = fSheldon (x= namesmengal16[namesmengal16$number != 0,],
k="number", n="population", location  = "muni",
s = names2$ni)
result

data(nameswomengal16)
names2=sqldf('select  muni, count(name) as ni,
sum(number) as population from nameswomengal16
group by muni;')

result = fSheldon (x= nameswomengal16[nameswomengal16$number != 0,],
k="number", n="population", location  = "muni",
s = names2$ni)
result
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ funciones }% __ONLY ONE__ keyword per line