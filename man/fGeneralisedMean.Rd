\encoding{latin1}
\name{fGeneralisedMean}
\alias{fGeneralisedMean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculate the Generalised Mean }
\description{
  This function obtains the generalised mean of relative abundances for a collection of species introduced by Angelika C. Studeny. It is a method for quantifying species biodiversity that can be adapted to the context of onomastics.
}
\usage{
fGeneralisedMean (x, pki, pki0, s, location, lambda)
}
\arguments{
  \item{x}{ dataframe of the data values for each species not null (because if you have a sample, there might be species that are not represented).}
  \item{pki}{ name of a variable which represents the relative frequency for each species.}
  \item{pki0}{ variable which represents the relative frequency for each species not null (because if you have a sample, there might be species that are not represented).}
  \item{location}{ name of a variable which represents the grouping element.}
  \item{s}{ vector which represents total number of species.}
  \item{lambda}{ free parameter.}
}
\details{
  For a community \eqn{i}{}, the generalised mean of relative abundances is defined by
\eqn{M_t (\lambda) = \left[\frac{1}{S_i} \sum_{k\in S_i} \left(\frac{N_{ki}^t}{N_{ki}^{t0}}\right)^\lambda\right]^{\frac{1}{\lambda}}}{},
where \eqn{N_{ki}^t}{} denotes the number of individuals of species \eqn{k}{} at times \eqn{t}{}, \eqn{t0}{} is the baseline year and \eqn{S_i}{} are all species at the community, species richness, and \eqn{\lambda}{} can be any non-zero real number.

In onomastic context, \eqn{N_{ki}^t}{} denotes the absolute frequency of surname \eqn{k}{} in region (\eqn{\approx}{} community diversity context) \eqn{i}{} at times \eqn{t}{}.
}
\value{
    A dataframe containing the following components:
    \item{\code{location}}{ represents the grouping element, for example the communities / regions.}
    \item{\code{generalisedMean}}{ the value of generalised mean.}
}
\references{
Studeny, A.C. (2012). \emph{Quantifying Biodiversity Trends in Time and Space}. PhD thesis, University of St Andrews.
}

\seealso{
\code{\link{fMargalef}}, 
\code{\link{fMenhinick}}, 
\code{\link{fPielou}},
\code{\link{fShannon}},
\code{\link{fSheldon}},
\code{\link{fSimpson}},
\code{\link{fSimpsonInf}},
\code{\link{fGeometricMean}},
\code{\link{fHeip}}}

\author{Maria Jose Ginzo Villamayor}

\examples{
library(sqldf)
data(surnamesgal14)

loc <- length(unique(surnamesgal14$muni))

apes2=sqldf('select  muni, count(surname) as ni,
sum(number) as population from surnamesgal14
group by muni;')

result = fGeneralisedMean(x= surnamesgal14[surnamesgal14$number != 0,],
pki="pki", pki0=surnamesgal14[surnamesgal14$number != 0,"pki"],
location  = "muni", s = apes2$ni[1:loc], lambda = 1 )
result

data(namesmengal16)

loc <- length(unique(namesmengal16$muni))

namesmengal16$pki <- (namesmengal16$number /
namesmengal16$population)

names2=sqldf('select  muni, count(name) as ni,
sum(number) as population from namesmengal16
group by muni;')

result = fGeneralisedMean(x= namesmengal16[namesmengal16$number != 0,],
pki="pki", pki0=namesmengal16[namesmengal16$number != 0,"pki"],
location  = "muni", s = names2$ni[1:loc], lambda = 1 )
result

data(nameswomengal16)

loc <- length(unique(nameswomengal16$muni))

nameswomengal16$pki <- (nameswomengal16$number /
nameswomengal16$population)

names2=sqldf('select  muni, count(name) as ni,
sum(number) as population from nameswomengal16
group by muni;')

result = fGeneralisedMean(x= nameswomengal16[nameswomengal16$number != 0,],
pki="pki", pki0=nameswomengal16[nameswomengal16$number != 0,"pki"],
location  = "muni", s = names2$ni[1:loc], lambda = 1 )
result
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ funciones }% __ONLY ONE__ keyword per line