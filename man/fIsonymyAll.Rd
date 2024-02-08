\encoding{latin1}
\name{fIsonymyAll}
\alias{fIsonymyAll}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculate the Isonymy, Isonymy between regions, Lasker distances, Euclidean distance and Nei's distances}
\description{
   This function obtains the Isonymy, Isonymy between regions, Lasker distance, Euclidean distance and Nei's distances and Hedrick's coefficient.
}
\usage{
fIsonymyAll (x, n, location, union, measure)
}
\arguments{
  \item{x}{ data frame with the data.}
  \item{n}{ number of the locations in the data frame.}
  \item{location}{ name of a variable which represents the location in the data.}
  \item{union}{ variable to be used to search for matching surnames in two locations.}
  \item{measure}{ name of a variable which represents the relative frequency for each surname.}
}
\details{
  Values of Isonymy, Isonymy between regions, Lasker distance, Euclidean distance and Nei's distances and Hedrick's coefficient.
 
Surname (dis)similarity among regions can be quantified by different measures. Consider index \eqn{i=1,\ldots,n}{} for denoting a certain geographical region (for two regions, \eqn{(i,j)}{}). Each region has an associated collection \eqn{S_i}{} of surnames, and for a pair of regions, the collection of all the surnames in them is denoted by \eqn{S_{ij} (S_{ij}=S_i\cup S_j)}{}. The total number of surnames in a certain region \eqn{i}{} is denoted by \eqn{n_i}{}. Surnames will be denoted by indices \eqn{k}{} and \eqn{l}{}.

Isonymy is defined as \eqn{I_i=\sum \limits _{k\in S_i}p_{ki}^2}{} where \eqn{p_{ki}}{} denotes the relative frequency of surname \eqn{k}{} in region \eqn{i}{}. Isonymy can be also extended as a measure of population similarities between groups. Under the assumption of a common origin, isonymy between two regions \eqn{i}{} and \eqn{j}{} is defined as \eqn{I_{ij}=\sum \limits_{k\in S_{ij}}p_{k_i}p_{k_j}}{}.

Other different measures of the isonymic distance between a pair of locations can be derived from isonymy between. For instance, the Lasker distance is given by \eqn{L = -\log(I_{ij})}{}.

Lasker distance can be interpreted as a measure of similarity between to areas, where large distance indicate less similarity in surname composition. Nevertheless, Lasker distance is not the only option to quantify surname similarity. Other common coefficients are the Euclidean distance and Nei's distance, both of them given by \eqn{E = \sqrt{1-\sum_{k\in S_{ij}}{\sqrt{p_{ki}p_{kj}}}}\quad\mbox{and}\quad N = -\log\left(\frac{I_{ij}}{\sqrt{I_iI_j}}\right),}{} respectively.
Finally, Hedrick's coefficient gives a standardized measure of isonymy using a procedure similar to that utilized in the calculation of a correlation coefficient. Specifically:
\eqn{H_{ij} = \frac{ 2 \sum \limits_{k \in S_{ij}} p_{ki} p_{kj}}{  
\left(\sum \limits_{k \in S_{ij}} p_{ki}^2 + \sum \limits_{k \in S_{ij}} p_{kj}^2 \right) } \mbox{, with } i,j=1\ldots,n.}{}


 In diversity context, \eqn{p_{ki}}{} denotes the relative frequency of species \eqn{k}{} in community (\eqn{\approx}{} region onomastic context) \eqn{i}{} and \eqn{S_i}{} are all species in community \eqn{i}{}.
}
\value{
    A list containing the following components:
    \item{\code{isonymy}}{ data frame with two columns and number of rows the number of regions / communities (\eqn{n}{}). For each location, it returns the value of the isonymy.}
    \item{\code{isonymy.btw}}{ the value of isonymy between. Matrix, \eqn{n \times n}{}.}
    \item{\code{hedrick}}{ the value of Hedrick's coefficient. Matrix, \eqn{n \times n}{}.}
    \item{\code{nei}}{ the value of Nei's distance. Matrix, \eqn{n \times n}{}.}
    \item{\code{lasker}}{ the value of Lasker distance. Matrix, \eqn{n \times n}{}.}
    \item{\code{distE}}{ the value of Euclidean distance. Matrix, \eqn{n \times n}{}.}
}
\references{
Barrai, I., Scapoli, C., Beretta, M., Nesti, C., Mamolini, E., and Rodriguez--Larralde, A., (1996) Isonymy and the genetic structure of Switzerland. I: The distributions of surnames. \emph{Annals of Human Biology}, \bold{23}, 431--455.

Cavalli-Sforza, L. L., and Edwards, A. W. F., (1967), Phylogenetic analysis models and estimation procedures. \emph{American Journal of Human Genetics}, \bold{19}, 233 257.

Hedrick, P. W. (1971), A new approach to measuring genetic similarity. \emph{Evolution}, \bold{25}: 276--280. 

Lasker, G. W. (1977) A coefficicnt of relationship by isonymy: a method for estimating the genetic relationship between populations. \emph{Human Biology}, \bold{49}, 489--493.

Mikerezi, I., Shina, E. Scapoli, C., Barbujani, G. Mamolini, E., Sandri, M., Carrieri, A., Rodriguez--Larralde, A. and Barrai, I. (2013). Surnames in Albania: a study of the population of Albania through isonymy. \emph{Annals of Human Genetics}, \bold{77}, 232--243.

Nei, M.(1973). The theory and estimation of genetic distance. In \emph{Genetic Structure of Populations}, edited by N. E. Morton, (Honolulu: University Press of Hawaii), 45--54.

Weiss, V. 1980. Inbreeding and genetic distance between hierarchically structured populations measured by surname frequencies. \emph{Mankind Quarterly}, \bold{21}, 135--149.

}

\seealso{\code{\link{fIsonymy}.}}

\author{Maria Jose Ginzo Villamayor}

\examples{
\donttest{
data(surnamesgal14)
result = fIsonymyAll (x= surnamesgal14, n= 314, location = 'muni',
union = 'surname', measure = 'pki')
result

data(namesmengal16)
namesmengal16$pki <- (namesmengal16$number /
namesmengal16$population)
result = fIsonymyAll (x= namesmengal16, n= 313, location = 'muni',
union = 'name', measure = 'pki')
result

data(nameswomengal16)
nameswomengal16$pki <- (nameswomengal16$number /
nameswomengal16$population)
result = fIsonymyAll (x= nameswomengal16, n= 313, location = 'muni',
union = 'name', measure = 'pki')
result
}}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ funciones }% __ONLY ONE__ keyword per line