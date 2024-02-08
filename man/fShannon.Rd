\encoding{latin1}
\name{fShannon}
\alias{fShannon}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculate the Shannon-Weaver diversity index }
\description{
  This function obtains the Shannon-Weaver diversity index introduced by Claude Elwood Shannon. This diversity measure came from information theory and measures the order (or disorder) observed within a particular system. It is a method for quantifying species biodiversity that can be adapted to the context of onomastics.
}
\usage{
 fShannon(x, k, n, location)
}
\arguments{
  \item{x}{ dataframe of the data values for each species not null (because if you have a sample, there might be species that are not represented)..}
  \item{k}{ name of a variable which represents absolute frequency for each species.}
  \item{n}{ name of a variable which represents total number of individuals.}
  \item{location}{ represents the grouping element.}
}
\details{
  For a community \eqn{i}{}, the index of Shannon-Weaver is defined by the expression
\eqn{H^{\prime} = -\sum\limits_{k\in S_i} (p_{ki} \log_2 p_{ki})}{}, where \eqn{p_{ki}}{} represents the relative frequency of species \eqn{k}{}, because \eqn{p_{ki} = \frac{N_{ki}}{N_i}}{}, (where \eqn{N_{ki}}{} denotes the number of individuals of species \eqn{k}{} and \eqn{N_i}{} total number of  individuals in all \eqn{S_i}{} species at the community, species richness. This index is related to the weighted geometric mean of the proportional abundances of the types.

In onomastic context, \eqn{p_{ki}}{} denotes the relative frequency of surname \eqn{k}{} in region (\eqn{\approx}{} community diversity context) \eqn{i}{} and \eqn{S_i}{} are all surnames in region \eqn{i}{}.
}
\value{
  A dataframe containing the following components:
    \item{\code{location}}{ represents the grouping element, for example the communities / regions.}
    \item{\code{shannon}}{ the value of the Shannon-Weaver diversity index.}
}
\references{
Shannon C.E. (1948). \emph{A mathematical theory of communication}. Bell System Technical Journal, \bold{27}, 379--423.

Shannon C.E., Weaver W. (1949). \emph{The Mathematical Theory of Communication}.
Urbana: University of Illinois Press. USA, \bold{96}. pp. 117. 
}

\seealso{
\code{\link{fMargalef}},
\code{\link{fMenhinick}},
\code{\link{fPielou}},
\code{\link{fSheldon}},
\code{\link{fSimpson}},
\code{\link{fSimpsonInf}},
\code{\link{fGeneralisedMean}}, \code{\link{fGeometricMean}},
\code{\link{fHeip}}.}

\author{Maria Jose Ginzo Villamayor}

\examples{
data(surnamesgal14)
result = fShannon (x= surnamesgal14[surnamesgal14$number != 0,],
k="number", n="population", location  = "muni" )
result

data(namesmengal16)
result = fShannon (x= namesmengal16[namesmengal16$number != 0,],
k="number", n="population", location  = "muni" )
result

data(nameswomengal16)
result = fShannon (x= nameswomengal16[nameswomengal16$number != 0,],
k="number", n="population", location  = "muni" )
result
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ funciones }% __ONLY ONE__ keyword per line