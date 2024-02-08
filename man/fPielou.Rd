\encoding{latin1}
\name{fPielou}
\alias{fPielou}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculate the Pielou's diversity index }
\description{
  This function obtains the Pielou's diversity index which is an index that measures diversity along with species richness introduced by Evelyn Chrystalla Pielou. It is a method for quantifying species biodiversity that can be adapted to the context of onomastic.
}
\usage{
fPielou(x, k, n, location, s)
}
\arguments{
  \item{x}{ dataframe of the data values for each species not null (because if you have a sample, there might be species that are not represented).}
  \item{k}{ name of a variable which represents absolute frequency for each species.}
  \item{n}{ name of a variable which represents total number of individuals.}
  \item{location}{ represents the grouping element.}
  \item{s}{ vector which represents number of species.}
}
\details{
  For a community \eqn{i}{}, the Pielou's diversity index is defined by
\eqn{J^{\prime} = \frac{H^{\prime}}{\log_2S_i}}{}, where \eqn{H^{\prime}}{} denotes the Shannon-Wiener index and
\eqn{\log_2S_i}{} denotes the maximum diversity \eqn{H^{\prime}_{\max}}{}.
Pielou's index is the Shannon-Weiner index computed for the sample \eqn{S_i}{} and represents a measure of Evenness of the community. If all species are represented in equal numbers in the sample, then \eqn{J^{\prime} = 1}{}. If one species strongly dominates \eqn{J^{\prime}}{} is close to zero.

In onomastic context, \eqn{S_i}{} are all surnames in region (\eqn{\approx}{} community diversity context) \eqn{i}{}.
}
\value{
   A dataframe containing the following components:
    \item{\code{location}}{ represents the grouping element, for example the communities / regions.}
    \item{\code{pielou}}{ the value of the Pielou's diversity index.}
}
\references{
Pielou, E. C. (1966) The measurement of diversity in different types of biological collections. \emph{Journal of Theoretical Biology}, \bold{13}, 131-144.
}

\seealso{
\code{\link{fMargalef}},
\code{\link{fMenhinick}},
\code{\link{fShannon}},
\code{\link{fSheldon}},
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

result = fPielou (x= surnamesgal14[surnamesgal14$number != 0,],
k="number", n="population", location  = "muni", s = apes2$ni )
result

data(namesmengal16)

names2=sqldf('select  muni, count(name) as ni,
sum(number) as population from namesmengal16
group by muni;')

result = fPielou (x= namesmengal16[namesmengal16$number != 0,],
k="number", n="population", location  = "muni", s = names2$ni )
result

data(nameswomengal16)

names2=sqldf('select  muni, count(name) as ni,
sum(number) as population from nameswomengal16
group by muni;')

result = fPielou (x= nameswomengal16[nameswomengal16$number != 0,],
k="number", n="population", location  = "muni", s = names2$ni )
result
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ funciones }% __ONLY ONE__ keyword per line