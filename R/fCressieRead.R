fCressieRead <-
function(x, number, population, ni, location, lambda){
  n <- length(x)
  if (n==0) stop("No observations)")
  if (number=="") stop("number is missing)")
  if (population=="") stop("population is missing)")
  if (ni=="") stop("ni is missing)")
  if (location=="") stop("location is missing)")
  if (lambda=="") stop("lambda is missing)")

  i <- aggregate(x[[number]] * ((x[[number]]  / (x[[population]]/x[[ni]] ))^lambda -1), by=list(Category=x[[location]]), FUN=sum)
  i[2] <- (2/(lambda*(lambda+1))) * i[2]
  names(i) <- c("location", "cressieRead")
  return(i)
}
