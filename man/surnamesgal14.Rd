\name{surnamesgal14}
\alias{surnamesgal14}
\docType{data}
\title{ surnamesgal14 data
}
\description{
This dataset corresponds to 25 most frequent surnames by municipality in Galicia in 2014.
}
\usage{
data(surnamesgal14)
}
\format{
  \code{surnamesgal14} is a data frame with surnames from Galicia in 2014.
}

\source{
The data corresponds to 25 most frequent surnames by municipality in Galicia in 2014. 
The dataset contains 8 columns, \code{prov}: the province, \code{muni}: the municipality, \code{namuni}: the name of the municipality, \code{surname}: the surname, \code{number}: the number of people with that surname, \code{population}: the total population considered by municipality, \code{ni}: the number of surnames considered and \eqn{p_{ki}}{} which is the frequency of surname \eqn{k}{} in municipality \eqn{i}{}.

These data have been extracted from the website of the Galician Institute of Statistics (IGE). The IGE offers information on the surnames and names of the population whose residence is in the Autonomous Community of Galicia. 
The base information for the elaboration data is the file of the Municipal Register of inhabitants of 2014 that the National Institute of Statistics (INE) provides to the IGE.
}
\references{
Galician Institute of Statistics (IGE), \href{https://www.ige.gal/igebdt/esq.jsp?idioma=gl&ruta=onomast/notamet.jsp}{https://www.ige.eu/}
}
\examples{
data(surnamesgal14)
}
\keyword{datasets}