#Hausdorff distance
dist <- function(A,B)
{
  n <- length(A)
  m <- length(B)
  d <- matrix(nrow = n, ncol = m)
  for (i in 1:n)
  {
    for (j in 1:m)
    {
      d[i,j] <- sqrt(sum((A[i] - B[j])^2))
    }
  }
  return(max((apply(d, 1, min)), (apply(d, 2, min))))
}