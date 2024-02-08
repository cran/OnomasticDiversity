\encoding{latin1}
\name{fHill}
\alias{fHill}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculate the Hill's diversity numbers }
\description{
  This function obtains the Hill's diversity numbers introduced by M. O. Hill. It is a method for quantifying species biodiversity that can be adapted to the context of onomastic.
}
\usage{
fHill(x, k, n, location, lambda)
}
\arguments{
  \item{x}{ dataframe of the data values for each species.}
  \item{k}{ name of a variable which represents absolute frequency for each species.}
  \item{n}{ name of a variable which represents total number of individuals.}
  \item{location}{ represents the grouping element.}
  \item{lambda}{ free parameter.}
}
\details{
  For a community \eqn{i}{}, the Hill's diversity numbers are defined by the expression
\eqn{J(\lambda) = \left(\sum \limits_{k\in S_i} p_{ki}^\lambda\right)^{\frac{1}{1-\lambda}}}{}
with the restriction \eqn{\lambda \geq 0}{} where \eqn{p_{ki}}{} represents the relative frequency of species \eqn{k}{} and \eqn{S_i}{} are all species at the community, species richness, and \eqn{\lambda} is a free parameter. (This is equivalent to the exponential of Renyi's generalised entropy). The Renyi entropy of order \eqn{\lambda}{}, where \eqn{\lambda \geq 0}{} and \eqn{\lambda \neq 1}{}, is defined as
\eqn{\mathrm{H}_{\lambda}(X)=\frac{1}{1-\lambda} \log \left(\sum \limits_{i=1}^{n} p_{i}^{\lambda}\right)}{}
Here, \eqn{X}{} is a discrete random variable with possible outcomes in the set \eqn{\mathcal{A}=\left\{x_{1}, x_{2}, \ldots, x_{n}\right\}}{} and corresponding probabilities \eqn{p_{i} \doteq \operatorname{Pr}\left(X=x_{i}\right)}{} for \eqn{i=1, \ldots, n}{}. The logarithm is conventionally taken to be base 2, especially in the context of information theory where bits are used. If the probabilities are \eqn{p_{i}=1 / n}{} for all \eqn{i=1, \ldots, n}{}, then all the Renyi entropies of the distribution are equal: \eqn{\mathrm{H}_{\lambda}(X)=\log n}{}. In general, for all discrete random variables \eqn{X, \mathrm{H}_{\lambda}(X)}{} is a non-increasing function in \eqn{\lambda}{}.. 

Particular cases of \eqn{\lambda}{} values: \eqn{\lambda = 0, J(0)=S_i}{}, it corresponds species richness; \eqn{\lambda = 1, J(1)=e^{H_{t}}}{}, it corresponds the exponential of Shannon's entropy; and \eqn{\lambda = 2, J(2)= D_{S_i}}{}, it corresponds the 'inverse' Simpson index.

In onomastic context, \eqn{p_{ki}}{} denotes the relative frequency of surname \eqn{k}{} in region (\eqn{\approx}{} community diversity context) \eqn{i}{} and \eqn{S_i}{} are all surnames in region \eqn{i}{}.
}
\value{
   A dataframe containing the following components:
    \item{\code{location}}{ represents the grouping element, for example the communities / regions.}
    \item{\code{hill}}{ the value of the Hill's diversity index.}
}
\references{
Hill, M. O. (1973). Diversity and Evenness: a unifying notation and its consequences. \emph{Ecology}, \bold{54}, 427--32.
}

\seealso{\code{\link{fCressieRead}.}}

\author{Maria Jose Ginzo Villamayor}

\examples{
data(surnamesgal14)
result = fHill (x= surnamesgal14, k="number", n="population",
location  = "muni", lambda= 0)
result

data(namesmengal16)
result = fHill (x= namesmengal16, k="number", n="population",
location  = "muni", lambda= 0)
result

data(nameswomengal16)
result = fHill (x= nameswomengal16, k="number", n="population",
location  = "muni", lambda= 0)
result
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ funciones }% __ONLY ONE__ keyword per line