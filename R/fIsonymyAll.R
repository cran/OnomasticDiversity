fIsonymyAll <-
function(x, n, location, union, measure){
  nx <- length(x)
  if (nx==0) stop("No observations)")
  if (n=="") stop("n is missing)")
  if (location=="") stop("location is missing)")
  if (union=="") stop("union is missing)")
  if (measure=="") stop("measure is missing)")


  isonymy.betw = matrix(0, n,n, byrow = TRUE)
  hedrick      = matrix(0, n,n, byrow = TRUE)


  isonymy <- aggregate(x[[measure]]^2, by=list(Category=x[[location]]), FUN=sum)



  diag(isonymy.betw) = 0 #isonymy[[2]]
  diag(hedrick) = 0

  nei = matrix(0, n,n, byrow = TRUE)
  diag(nei) = 0
  lasker = matrix(0, n,n, byrow = TRUE)
  diag(lasker) = 0
  distE = matrix(0, n,n, byrow = TRUE)
  diag(distE) = 0


    for (i in 1:(n-1)){

      aux1 <- x[x[[location]] == i,]


      for (j in (i+1):n){


        aux2 <- x[x[[location]] == j,] # Tengo dos municipios.

        aux3 <- merge(aux1[, c(measure, union)], aux2[, c(measure, union)], by = c(union, union))

        if(dim(aux3)[1] == 0)
        {
          isonymy.betw[i,j] = 0
          hedrick[i,j] = 0
          nei[i,j] = NA
          lasker[i,j] = NA
          distE[i,j] = 1
        }
        else
        {
          isonymy.betw[i,j] =  sum(aux3[,2] *  aux3[,3])

          nei[i,j]    = -log(isonymy.betw[i,j] / sqrt(isonymy[[2]][i]*isonymy[[2]][j]))

          hedrick[i,j]= -log(isonymy.betw[i,j] / (0.5*(isonymy[[2]][i]+isonymy[[2]][j])))

          lasker[i,j] = -log(isonymy.betw[i,j])

          distE[i,j] = sqrt(1- as.numeric(sum(sqrt(aux3[,2] *  aux3[,3]))))
        }

        isonymy.betw[j,i] = isonymy.betw[i,j]
        nei[j,i]          = nei[i,j]
        hedrick[j,i]      = hedrick[i,j]
        lasker[j,i]       = lasker[i,j]
        distE[j,i]        = distE[i,j]




      }

    }
  isonymy_list <- list("isonymy" = isonymy, "isonymy.betw" = isonymy.betw , "hedrick"=hedrick, "nei" = nei, "lasker" = lasker, "distE" = distE)
  return(isonymy_list)
}
