Runs <- function(x,nb) 
{
  #Obtention de la séquence concaténée
  V = binary(x[1])
  V = V[(32-nb+1):32]
  
  for(i in 2:length(x)) {
    bin = binary(x[i])
    V = c(V,bin[(32-nb+1):32])
  }
  n <- length(V)
  #pre-test
  pi <- sum(V)/n
  tau <- 2/sqrt(n)
  Pvaleur <- 0
  if(abs(pi-0.5)<tau)
  {
    Vnobs <- 1
    for(j in 1:(n-1))
    {
      if(V[j]!=V[j+1]){
        Vnobs <- Vnobs + 1
      }
    }
    Pvaleur <- 2*(1-pnorm(abs(Vnobs-2*n*pi*(1-pi))/(2*sqrt(n)*pi*(1-pi))))
  }
  return(Pvaleur)
}

Frequency <- function(x,nb)  
{
  s<-0
  for(i in 1:length(x))
  {
    bin=binary(x[i])
    for(j in 0:(nb-1))
    {
      s <- s + (2*bin[32-j]-1)
    }
  }
  Sobs <- abs(s)/sqrt(nb*length(x))
  Pvaleur <- 2*(1-pnorm(Sobs))
  return(Pvaleur)
}