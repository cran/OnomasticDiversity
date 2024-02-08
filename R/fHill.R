fHill <-
function(x, k, n, location, lambda){
  nx <- length(x)
  if (nx==0) stop("No observations)")
  if (k=="") stop("k is missing)")
  if (n=="") stop("n is missing)")
  if (location=="") stop("location is missing)")
  if (lambda=="") stop("lambda is missing)")

  j <- (aggregate((x[[k]] / x[[n]])^lambda, by=list(Category=x[[location]]), FUN=sum))
  j[[2]] <- j[[2]]^(1/(1-lambda))
  names(j) <- c("location", "hill")
  return(j)
}
