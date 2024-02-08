\encoding{latin1}
\name{fCressieRead}
\alias{fCressieRead}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{  Cressie and Read }
\description{
  This function obtains the Cressie and Read statistics introduced by Noel Cressie and Timothy Read. It is a method for quantifying species biodiversity that can be adapted to the context of onomastics.
}
\usage{
fCressieRead(x, number, population, ni, location, lambda)
}
\arguments{
  \item{x}{ dataframe of the data values.}
  \item{number}{ name of a variable which represents  number of individuals of each species.}
  \item{population}{ name of variable which represents total number of individuals.}
  \item{ni}{ name of variable which represent number of species.}
  \item{location}{ name of variable which represent represents the grouping element.}
  \item{lambda}{ free parameter.}
}
\details{
  For a community \eqn{i}{}, Cressie and Read (1984) introduced the following parametric form for a generalised statistic
\eqn{I_n (\lambda) = \frac{2}{\lambda(\lambda+1)}  \sum_{k\in S_i} { n_{ki} \left[ \left(\frac{n_{ki}}{n/S_i}\right)^\lambda-1\right]}}{}, where \eqn{n_{ki}}{} represents the number of individuals of species \eqn{k}{} in a sample (in the population is \eqn{N_{ki}}{}), \eqn{S_i}{} represents all species at the community, species richness, and \eqn{\lambda}{} is a free parameter.

Varying the value of \eqn{\lambda}{} gets different statistics. 
If \eqn{\lambda= -1}{} and \eqn{\lambda= 0}{}, \eqn{I_n(\lambda)}{} is not defined, but in any case, limits \eqn{\lambda = -1}{} and \eqn{\lambda = 0}{} can be taken.

In onomastic context, \eqn{n_{ki}}{} (\eqn{\approx N_{ki}}{}) denotes the absolute frequency of surname \eqn{k}{} in region \eqn{i}{} (\eqn{\approx}{} community diversity context \eqn{i}{}).
}
\value{
  A dataframe containing the following components:
    \item{location}{ represents the grouping element, for example the communities / regions.}
    \item{cressieRead}{ the value of Cressie and Read statistics.}
}
\references{
Cressie, Noel and Read, Timothy RC (1984) Multinomial goodness-of-fit tests. \emph{Computational Statistics and Data Analysis}, \bold{46}(3), 440--464.
}

\seealso{\code{\link{fHill}}}

\author{Maria Jose Ginzo Villamayor}

\examples{
data(surnamesgal14)
result = fCressieRead(x= surnamesgal14 , number="number",
population="population", location = "muni", ni="ni",
lambda = 2)
result
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ funciones }% __ONLY ONE__ keyword per line