fShannon <-
function(x, k, n, location){
  nx <- length(x)
  if (nx==0) stop("No observations)")
  if (k=="") stop("k is missing)")
  if (n=="") stop("n is missing)")
  if (location=="") stop("location is missing)")

  h <- aggregate((-1)*(x[[k]] / x[[n]])*log2(x[[k]] / x[[n]]), by=list(Category=x[[location]]), FUN=sum)
  names(h) <- c("location", "shannon")
  return(h)
}
