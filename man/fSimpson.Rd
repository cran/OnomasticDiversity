\encoding{latin1}
\name{fSimpson}
\alias{fSimpson}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculate the Simpson's diversity index }
\description{
  This function obtains the Simpson's diversity index and the inverse introduced by Edward Hugh Simpson. It was the first index used in ecology. It is a method for quantifying species biodiversity that can be adapted to the context of onomastics.
}
\usage{
fSimpson(x, k, n, location)
}
\arguments{
  \item{x}{ dataframe of the data values for each species.}
  \item{k}{ name of a variable which represents absolute frequency for each species}
  \item{n}{ name of a variable which represents total number of individuals.}
  \item{location}{ represents the grouping element.}
}
\details{
  For a community \eqn{i}{}, the Simpson's diversity index is defined by
\eqn{D_{S_i} = \sum \limits_{k\in S_i} p_{ki}^2}{}, where \eqn{p_{ki}}{}  represents the relative frequency of species \eqn{k}{}, because \eqn{p_{ki} = \frac{N_{ki}}{N_i}}{}, (where \eqn{N_{ki}}{} denotes the number of individuals of species \eqn{k}{} and \eqn{N_i}{} total number of individuals in all \eqn{S_i}{} species at the community, species richness. The Simpson index tends to be smaller when the community is more diverse.

In onomastic context, \eqn{p_{ki}}{} denotes the relative frequency of surname \eqn{k}{} in region (\eqn{\approx}{} community diversity context) \eqn{i}{}, i.e., Simpson's diversity index is equivalent to the concept of isonymy..
}
\value{
  A dataframe containing the following components:
    \item{\code{location}}{ represents the grouping element, for example the communities / regions.}
    \item{\code{simpson}}{ the value of the Simpson's diversity index.}
    \item{\code{divSimpson}}{ the value of the inverse Simpson's diversity index.}
}
\references{
Simpson (1949) Measurement of diversity. \emph{Nature}, \bold{163}.
}

\seealso{
\code{\link{fMargalef}},
\code{\link{fMenhinick}},
\code{\link{fPielou}},
\code{\link{fShannon}},
\code{\link{fSheldon}}, 
\code{\link{fSimpsonInf}},
\code{\link{fGeneralisedMean}}, \code{\link{fGeometricMean}},
\code{\link{fHeip}}.}

\author{Maria Jose Ginzo Villamayor}

\examples{
data(surnamesgal14)
result = fSimpson (x= surnamesgal14, k="number",
n="population", location  = "muni" )
result

data(namesmengal16)
result = fSimpson (x= namesmengal16, k="number",
n="population", location  = "muni" )
result

data(nameswomengal16)
result = fSimpson (x= nameswomengal16, k="number",
n="population", location  = "muni" )
result
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ funciones }% __ONLY ONE__ keyword per line