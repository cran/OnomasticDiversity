fMenhinick <-
function(x, s, n, location){
  nx <- length(x)
  if (nx==0) stop("No observations)")
  if (s=="") stop("s is missing)")
  if (n=="") stop("n is missing)")
  if (location=="") stop("location is missing)")

  R2 <- data.frame(x[[location]], x[[s]] / sqrt(x[[n]]))
  names(R2) <- c("location", "menhinick")
  return(R2)
}
