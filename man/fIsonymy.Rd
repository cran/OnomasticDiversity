\encoding{latin1}
\name{fIsonymy}
\alias{fIsonymy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculate the Isonymy within a region}
\description{
  This function obtains the isonymy within a region \eqn{i}{} which has an associated collection \eqn{S_i}{} of surnames.
}
\usage{
fIsonymy(x, category)
}
\arguments{
  \item{x}{ a vector of relative frequency squared for each surname.}
  \item{category}{ represents the grouping element, for example the regions.}
}
\details{
  Isonymy is defined as \eqn{I_i=\sum\limits_{k\in S_i}p_{ki}^2}{} where \eqn{p_{ki}}{} denotes the relative frequency of surname \eqn{k}{} in region \eqn{i}{}.
 
 In diversity context, \eqn{p_{ki}}{} denotes the relative frequency of species \eqn{k}{} in community (\eqn{\approx}{} region onomastic context) \eqn{i}{} and \eqn{S_i}{} are all species in community \eqn{i}{}.
}
\value{
  A dataframe containing the following components:
    \item{\code{category}}{ represents the grouping element, for example the regions / communities.}
    \item{\code{x}}{ the value of isonymy.}
}
\references{
Crow J.F. and Mange A.P., (1965). Measurement of inbreeding from the frequency of marriages between persons of the same surname. \emph{Eugenics Quarterly}, \bold{12}(4), 199--203.

Barrai, I., Scapoli, C., Beretta, M., Nesti, C., Mamolini, E., and Rodriguez--Larralde, A., (1996). Isonymy and the genetic structure of Switzerland. I: The distributions of surnames. \emph{Annals of Human Biology}, \bold{23}, 431--455.
}

\seealso{\code{\link{fIsonymyAll}}.}

\author{Maria Jose Ginzo Villamayor}

\examples{
data(surnamesgal14)
surnamesgal14$pki2 <- (surnamesgal14$number / surnamesgal14$population)^2
result = fIsonymy(surnamesgal14$pki2, surnamesgal14$namuni)
result

data(namesmengal16)
namesmengal16$pki2 <- (namesmengal16$number / namesmengal16$population)^2
result = fIsonymy(namesmengal16$pki2, namesmengal16$namuni)
result

data(nameswomengal16)
nameswomengal16$pki2 <- (nameswomengal16$number / nameswomengal16$population)^2
result = fIsonymy(nameswomengal16$pki2, nameswomengal16$namuni)
result
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ funciones }% __ONLY ONE__ keyword per line