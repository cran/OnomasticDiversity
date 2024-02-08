fSimpson <-
function(x, k, n, location){
  nx <- length(x)
  if (nx==0) stop("No observations)")
  if (k=="") stop("k is missing)")
  if (n=="") stop("n is missing)")
  if (location=="") stop("location is missing)")

  h <- aggregate((x[[k]] / x[[n]])^2, by=list(Category=x[[location]]), FUN=sum)
  h <- data.frame(h, 1-h[2])
  names(h) <- c("location", "simpson", "divSimpson")
  return(h)
}
