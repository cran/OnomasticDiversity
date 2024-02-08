\encoding{latin1}
\name{fMenhinick}
\alias{fMenhinick}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculate the Menhinick's diversity index }
\description{
  This function obtains the Menhinick's diversity index introduced by Edward F. Menhinick. It is a method for quantifying species biodiversity that can be adapted to the context of onomastic.
}
\usage{
fMenhinick(x, s, n, location)
}
\arguments{
  \item{x}{ dataframe which contains the number of species and population for each location.}
  \item{s}{ name of a variable which represents number of species.}
  \item{n}{ name of a variable which represents total number of individuals.}
  \item{location}{ name of a variable which represents represents the grouping element.}
}
\details{
  For a community \eqn{i}{}, the Menhinick's diversity index is defined by
\eqn{R_2 = \frac{s_i}{\sqrt{N_i}}}{}, where \eqn{s_i}{} represents the number of species (richness) and \eqn{N_i}{} represents the total number of  individuals in all \eqn{s_i}{}.

In onomastic context, \eqn{N_i}{} denotes the number of  individuals in region  (\eqn{\approx}{} community diversity context) \eqn{i}{} and \eqn{s_i}{} represents the total number of surnames.
}
\value{
   A dataframe containing the following components:
    \item{\code{location}}{ represents the grouping element, for example the communities / regions.}
    \item{\code{menhinick}}{ the value of the Menhinick's diversity index.}
}
\references{
Menhinick E.F. (1964) A comparison of some species-individuals diversity indices applied to samples of field insects. \emph{Ecology}, \bold{45}, 859--861.
}

\seealso{
\code{\link{fMargalef}},
\code{\link{fPielou}},
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

result = fMenhinick(x= apes2, s="ni", n="population",
location  = "muni")
result

data(namesmengal16)

names2=sqldf('select  muni, count(name) as ni,
sum(number) as population from namesmengal16
group by muni;')

result = fMenhinick(x= names2, s="ni", n="population",
location  = "muni")
result

data(nameswomengal16)

names2=sqldf('select  muni, count(name) as ni,
sum(number) as population from nameswomengal16
group by muni;')

result = fMenhinick(x= names2, s="ni", n="population",
location  = "muni")
result
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ funciones }% __ONLY ONE__ keyword per line