\encoding{latin1}
\name{fHeip}
\alias{fHeip}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculate the Heip's diversity index }
\description{
  This function obtains the Heip's diversity index introduced by Carlo H. R. Heip. It is a method for quantifying species biodiversity that can be adapted to the context of onomastic.
}
\usage{
fHeip (x, k, n, location, s)
}
\arguments{
  \item{x}{  dataframe of the data values for each species not null (because if you have a sample, there might be species that are not represented).}
  \item{k}{ name of a variable which represents absolute frequency for each species.}
  \item{n}{ name of a variable which represents total number of individuals.}
  \item{location}{ represents the grouping element.}
  \item{s}{ vector which represents total number of species.}
}
\details{
  For a community \eqn{i}{}, the Heip's diversity index is defined by
\eqn{E_{He} = \frac{2^{H^{\prime}}-1}{S_i-1}}{}
where \eqn{H^{\prime}}{} is the Shannon diversity index and \eqn{S_i}{} are all species at the community, species richness. This index varies from 0 to 1 and measures how equally the species richness contributes to the total abundance of the community.

In onomastic context, \eqn{S_i}{} are all surnames in region (\eqn{\approx}{} community diversity context) \eqn{i}{}.
}
\value{
  A dataframe containing the following components:
    \item{\code{location}}{ represents the grouping element, for example the communities / regions.}
    \item{\code{heip}}{ the value of the Heip's diversity index.}
}
\references{
Heip, C. (1974). A New Index Measuring Evenness. \emph{Journal of the Marine Biological Association of the United Kingdom}, \bold{54}(3), 555--557.
}

\seealso{
\code{\link{fMargalef}},
\code{\link{fMenhinick}},
\code{\link{fPielou}},
\code{\link{fShannon}},
\code{\link{fSheldon}},
\code{\link{fSimpson}},
\code{\link{fSimpsonInf}},
\code{\link{fGeneralisedMean}}, \code{\link{fGeometricMean}}.}

\author{Maria Jose Ginzo Villamayor}

\examples{
library(sqldf)
data(surnamesgal14)
loc <- length(unique(surnamesgal14$muni))


apes2=sqldf('select  muni, count(surname) as ni,
sum(number) as population from surnamesgal14
group by muni;')


result = fHeip (x= surnamesgal14[surnamesgal14$number != 0,],
k="number", n="population", location  = "muni",
s = apes2$ni[1:loc] )
result

data(namesmengal16)
loc <- length(unique(namesmengal16$muni))


names2=sqldf('select  muni, count(name) as ni,
sum(number) as population from namesmengal16
group by muni;')


result = fHeip (x= namesmengal16[namesmengal16$number != 0,],
k="number", n="population", location  = "muni",
s = names2$ni[1:loc] )
result


data(nameswomengal16)
loc <- length(unique(nameswomengal16$muni))


names2=sqldf('select  muni, count(name) as ni,
sum(number) as population from nameswomengal16
group by muni;')


result = fHeip (x= nameswomengal16[nameswomengal16$number != 0,],
k="number", n="population", location  = "muni",
s = names2$ni[1:loc] )
result
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ funciones }% __ONLY ONE__ keyword per line