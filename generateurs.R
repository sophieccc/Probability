Rejet <- function(n)
{
  c = 2/(log(2)^2)
  X = matrix(nrow=n, ncol=1)
  for (i in 1:n)
  {
    U = runif(n,min = 0,max = 1)
    Y = runif(n,min = 0,max = 1)
    while(U > log(1+Y)/(1+Y) )
    {
      U = runif(n,min = 0,max = 1)
      Y = runif(n,min = 0,max = 1)
    }
    X[i]=Y
  }
  return(X)
}

f <- function(x)
{
  c = 2/(log(2)^2)
  y = c * log(1+x)/(1+x) * fancy_one (x)
  return (y)
}

fancy_one <- function(x)
{
  y=0;
  if(x>=0 && x<=1)
  {
    y=1;
  }
  return (y);
}

Inversion <- function(n)
{
  U = runif(n,min = 0,max = 1)
  X = exp(sqrt(U)*log(2))-1;
  return (X)
}


LoiBinomiale <- function (n,p)
{
  U = runif(n,min = 0,max = 1)
  X = sum(U < p)
  return (X)
}


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


RANDU <- function(k,graine)
{
  suite = matrix(nrow=k, ncol=1)
  suite[1]=graine
  for (i in 2:k)
  {
    suite[i]=(65539*suite[i-1])%%(2^31)
  }
  return(suite)
}


STANDARD_MINI <- function(k,graine)
{
  suite = matrix(nrow=k, ncol=1)
  suite[1]=graine
  for (i in 2:k)
  {
    suite[i]=(16807*suite[i-1])%%((2^31)-1)
  }
  return(suite)
}


VonNeumann <- function(n, p=1, graine)
{
  x <-  rep(graine,n*p+1)
  for(i in 2:(n*p+1))
  {
    numbers <- strsplit(format(x[i-1]^2,scientific=FALSE),'')[[1]]
    while(length(numbers)>4){ 
      numbers <- numbers[2:(length(numbers)-1)] 
    }
    x[i] <- as.numeric(numbers)%*%(10^seq(length(numbers)-1,0,-1))
  }
  x <- matrix(x[2:(n*p+1)],nrow=n,ncol=p)
  return(x)
}


MersenneTwister <- function(n, p=1, graine)
{
  set.seed(graine,kind='Mersenne-Twister')
  x <- sample.int(2^32-1,n*p)
  x <- matrix(x,nrow=n,ncol=p)
  return(x)
}


binary <- function(x)
{
  if((x<2^31)&(x>=0))
    return( as.integer(rev(intToBits(as.integer(x)))) )
  else{
    if((x<2^32)&(x>0))
      return( c(1,binary(x-2^31)[2:32]) )
    else{
      cat('Erreur dans binary : le nombre etudie n est pas un entier positif en 32 bits.\n')
      return(c())
    }
  }
}

