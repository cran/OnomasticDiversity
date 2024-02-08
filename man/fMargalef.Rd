\encoding{latin1}
\name{fMargalef}
\alias{fMargalef}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculate the Margalef's diversity index }
\description{
  This function obtains the Margalef's diversity index which is a species diversity index developed by Ramon Margalef Lopez during the 1950s. It is a method for quantifying species biodiversity that can be adapted to the context of onomastic.
}
\usage{
fMargalef(x, s, n, location)
}
\arguments{
  \item{x}{ dataframe which contains the number of species and population for each location.}
  \item{s}{ name of a variable which represents number of species.}
  \item{n}{ name of a variable which represents total number of individuals.}
  \item{location}{  name of a variable which represents represents the grouping element. }
}
\details{
  For a community \eqn{i}{}, the Margalef's diversity index is defined by
\eqn{R_1 = \frac{S_i-1}{\ln(N_i)}}{}, where \eqn{S_i}{} represents the number of species (richness) and \eqn{N_i}{} represents the total number of  individuals in all \eqn{S_i}{}.

In onomastic context, \eqn{N_i}{} denotes the number of  individuals in region  (\eqn{\approx}{} community diversity context) \eqn{i}{} and \eqn{S_i}{} represents the total number of surnames.
}
\value{
   A dataframe containing the following components:
    \item{\code{location}}{ represents the grouping element, for example the communities / regions.}
    \item{\code{margalef}}{ the value of the Margalef's diversity index.}
}
\references{
Margalef D.R., (1958), Information theory in ecology. \emph{International Journal of General Systems}, \bold{3}, 36--71.
}

\seealso{
\code{\link{fMenhinick}},
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

result = fMargalef (x= apes2, s="ni", n="population", location  = "muni")
result

data(namesmengal16)

names2=sqldf('select  muni, count(name) as ni,
sum(number) as population from namesmengal16
group by muni;')

result = fMargalef (x= names2, s="ni", n="population", location  = "muni")
result

data(nameswomengal16)

names2=sqldf('select  muni, count(name) as ni,
sum(number) as population from nameswomengal16
group by muni;')

result = fMargalef (x= names2, s="ni", n="population", location  = "muni")
result
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ funciones }% __ONLY ONE__ keyword per line