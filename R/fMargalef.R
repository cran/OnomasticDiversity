fMargalef <-
function(x, s, n, location){
  nx <- length(x)
  if (nx==0) stop("No observations)")
  if (s=="") stop("s is missing)")
  if (n=="") stop("n is missing)")
  if (location=="") stop("location is missing)")

  R1 <- data.frame(x[[location]], (x[[s]] -1) / log(x[[n]]))
  names(R1) <- c("location", "margalef")
  return(R1)
}
