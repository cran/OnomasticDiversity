fGeneralisedMean <-
function(x, pki, pki0, s, location, lambda){
  n <- length(x)
  if (n==0) stop("No observations)")
  if (pki=="") stop("pki is missing")
  if (length(pki0) == 0) stop("pki0 is missing")
  ns <- length(s)
  if (ns=="") stop("s is missing")
  if (location=="") stop("location is missing")
  if (lambda=="") stop("lambda is missing")

  m <- aggregate((x[[pki]] / pki0)^lambda, by=list(Category=x[[location]]), FUN=sum)
  m[2] <- ((1/s)*m[2])^(1/lambda)
  names(m) <- c("location", "generalisedMean")
  return(m)
}
