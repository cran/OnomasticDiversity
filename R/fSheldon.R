fSheldon <-
function(x, k, n, location, s){
  nx <- length(x)
  if (nx==0) stop("No observations)")
  if (k=="") stop("k is missing)")
  if (n=="") stop("n is missing)")
  if (location=="") stop("location is missing)")
  ns <- length(s)
  if (ns==0) stop("s is missing)")

  h <- fShannon (x=x, k=k, n=n, location=location )
  e <- data.frame(h[1], 2^h[2]/(s))
  names(e) <- c("location", "sheldon")
  return(e)
}
