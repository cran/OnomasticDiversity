fPielou <-
function(x, k, n, location, s){
  nx <- length(x)
  if (nx==0) stop("No observations)")
  if (k=="") stop("k is missing)")
  if (n=="") stop("n is missing)")
  if (location=="") stop("location is missing)")
  ns <- length(s)
  if (ns=="") stop("s is missing)")

  h <- fShannon (x=x, k=k, n=n, location=location )
  j <- data.frame(h[1], h[2]/log2(s))
  names(j) <- c("location", "pielou")
  return(j)
}
