fIsonymy <-
function(x, category){
  n <- length(x)
  if (n==0) stop("No observations)")
  nc <- length(category)
  if (nc==0) stop("category is missing)")

  iso <- aggregate(x, by=list(Category=category), FUN=sum)
  return(iso)
}
