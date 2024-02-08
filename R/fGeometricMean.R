fGeometricMean <-
function(x, pki, pki0, s, location){
  n <- length(x)
  if (n==0) stop("No observations)")
  if (pki=="") stop("pki is missing)")
  if (pki0=="") stop("pki0 is missing)")
  ns <- length(s)
  if (ns=="") stop("s is missing)")
  if (location=="") stop("location is missing)")

  g <- aggregate(log(x[[pki]] / x[[pki0]]), by=list(Category=x[[location]]), FUN=sum)
  g[2] <- exp((1/s)*g[2])
  names(g) <- c("location", "geometricMean")
  return(g)
}
