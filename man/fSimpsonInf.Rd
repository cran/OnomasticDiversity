\encoding{latin1}
\name{fSimpsonInf}
\alias{fSimpsonInf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculate the Simpson's diversity index and the inverse }
\description{
  This function obtains the Simpson's diversity index and the inverse introduced by Edward Hugh Simpson. It is a method for quantifying species biodiversity that can be adapted to the context of onomastics.
}
\usage{
fSimpsonInf(x, k, n, location)
}
\arguments{
  \item{x}{ dataframe of the data values for each species.}
  \item{k}{ name of a variable which represents absolute frequency for each species.}
  \item{n}{ name of a variable which represents total number of individuals.}
  \item{location}{ represents the grouping element.}
}
\details{
  For a community \eqn{i}{}, the Simpson (when \eqn{N_i}{} is not finite, data are assumed to come from a sample of size \eqn{N_i}{}) diversity index is defined by
\eqn{D^{\prime}_{S_i} = \sum \limits_{k\in S_i} \frac{n_{ki}(n_{ki}-1)}{n_i(n_i-1)}}{}, where \eqn{n_{ki}}{} represents the number of individuals of species \eqn{k}{} in a sample (in the total is \eqn{N_{ki}}{}) and \eqn{S_i}{} represents all species at the community, species richness. 

In onomastic context, \eqn{n_{ki}}{} (\eqn{\approx N_{ki}}{}) denotes the absolute frequency of surname \eqn{k}{} in region \eqn{i}{} 
and \eqn{S_i}{} are all surnames in region (\eqn{\approx}{} community diversity context) \eqn{i}{}.
}
\value{
  A dataframe containing the following components:
    \item{\code{location}}{ represents the grouping element, for example the communities / regions.}
    \item{\code{simpson}}{ the value of the Simpson's Diversity Index.}
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
\code{\link{fSimpson}},
\code{\link{fGeneralisedMean}}, \code{\link{fGeometricMean}},
\code{\link{fHeip}}.}


\author{Maria Jose Ginzo Villamayor}

\examples{
data(surnamesgal14)
result = fSimpsonInf (x= surnamesgal14, k="number",
n="population", location  = "muni" )
result

data(namesmengal16)
result = fSimpsonInf (x= namesmengal16, k="number",
n="population", location  = "muni" )
result

data(nameswomengal16)
result = fSimpsonInf (x= nameswomengal16, k="number",
n="population", location  = "muni" )
result
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ funciones }% __ONLY ONE__ keyword per line