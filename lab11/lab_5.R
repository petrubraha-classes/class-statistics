generation <- function(x, p)
{
  if(sum(p) != 1)
    return(-1) # error 
  
  u <- runif(1)
  sum <- cumsum(p)
  for(i in 1:length(x))
    if(u <= sum[i])
      return(x[i])

  return(x[length(x)])
}

ex1 <- function()
{
  temp = vector()
  for(i in 1:1000)
    temp[i] <- generation(c(1, 2, 3), c(0.2, 0.5, 0.3))
  distribution <- table(temp)
  cat("output:", distribution, "\n")
}
