fSimpsonInf <-
function(x, k, n, location){
  nx <- length(x)
  if (nx==0) stop("No observations)")
  if (k=="") stop("k is missing)")
  if (n=="") stop("n is missing)")
  if (location=="") stop("location is missing)")

  h <- aggregate((x[[k]] * (x[[k]] -1) / (x[[n]]*(x[[n]] -1)) ), by=list(Category=x[[location]]), FUN=sum)
  names(h) <- c("location", "simpson")
  return(h)
}
