\encoding{latin1}
\name{fGeometricMean}
\alias{fGeometricMean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculate the Geometric Mean }
\description{
  This function obtains the geometric mean introduced by Stephen Terrence Buckland and coauthors. It is a method for quantifying species biodiversity that can be adapted to the context of onomastic.
}
\usage{
fGeometricMean(x, pki, pki0, s, location)
}
\arguments{
  \item{x}{ dataframe of the data values for each species not null (because if you have a sample, there might be species that are not represented).}
  \item{pki}{ name of a variable which represents the relative frequency for each species.}
  \item{pki0}{  name of a variable which represents the relative frequency for each species at initial time point.}
  \item{s}{ vector which represents total number of species.}
  \item{location}{ represents the grouping element.}
}
\details{
  For a community \eqn{i}{}, the geometric mean of relative abundances is defined by
\eqn{G_t = \exp \left(\frac{1}{S_i} \sum_{k\in S_i} \log \frac{N_{ki}^t}{N_{ki}^{t_0}}\right)}{}, where \eqn{N_{ki}^t}{} denotes the number of individuals of species \eqn{k}{} at times $t$, \eqn{t_0}{} is the baseline year and \eqn{S_i}{} are all species at the community, species richness.

In onomastic context, \eqn{N_{ki}^t}{} denotes the absolute frequency of surname \eqn{k}{} in region (\eqn{\approx}{} community diversity context) \eqn{i}{} at times \eqn{t}{}.
}
\value{
  A dataframe containing the following components:
    \item{\code{location}}{ represents the grouping element, for example the communities / regions.}
    \item{\code{geometricMean}}{ the value of geometric mean.}
}
\references{
Buckland, S.T., Studeny, A.C., Magurran, A.E., Illian, J.B., & Newson, S.E.  (2011). The geometric mean of relative abundance indices: a biodiversity measure with a difference. \emph{Ecosphere}, \bold{2}(9), art.100.

Studeny, A.C. (2012). \emph{Quantifying Biodiversity Trends in Time and Space}. PhD thesis, University of St Andrews.

van Strien, A.J., Soldaat, L.L., & Gregory, R.D. (2012). Desirable mathematical properties of indicators for biodiversity change. \emph{Ecological Indicators}, \bold{14}, 202--208.
}

\seealso{
\code{\link{fMargalef}},
\code{\link{fMenhinick}},
\code{\link{fPielou}},
\code{\link{fShannon}},
\code{\link{fSheldon}}, 
\code{\link{fSimpson}},
\code{\link{fSimpsonInf}},
\code{\link{fGeneralisedMean}},
\code{\link{fHeip}}}

\author{Maria Jose Ginzo Villamayor}

\examples{
library(sqldf)
data(surnamesgal14)
loc <- length(unique(surnamesgal14$muni))

apes2=sqldf('select  muni, count(surname) as ni,
sum(number) as population from surnamesgal14
group by muni;')
surnamesgal14$pki0 <- surnamesgal14$pki

result = fGeometricMean (x= surnamesgal14[surnamesgal14$number != 0,],
pki="pki", pki0="pki0" , location  = "muni",
s = apes2$ni[1:loc])
result

data(namesmengal16)
loc <- length(unique(namesmengal16$muni))

names2=sqldf('select  muni, count(name) as ni,
sum(number) as population from namesmengal16
group by muni;')

namesmengal16$pki <- (namesmengal16$number /
namesmengal16$population)
namesmengal16$pki0 <- namesmengal16$pki

result = fGeometricMean (x= namesmengal16[namesmengal16$number != 0,],
pki="pki", pki0="pki0" , location  = "muni",
s = names2$ni[1:loc])
result

data(nameswomengal16)
loc <- length(unique(nameswomengal16$muni))

names2=sqldf('select  muni, count(name) as ni,
sum(number) as population from nameswomengal16
group by muni;')

nameswomengal16$pki <- (nameswomengal16$number /
nameswomengal16$population)
nameswomengal16$pki0 <- nameswomengal16$pki

result = fGeometricMean (x= nameswomengal16[nameswomengal16$number != 0,], 
pki = "pki", pki0 = "pki0", location  = "muni", 
s = names2$ni[1:loc])
result
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ funciones }% __ONLY ONE__ keyword per line