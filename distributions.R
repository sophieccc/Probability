Rejet <- function(n)
{
  c = 2/(log(2)^2)
  X = c()
  for (i in 1:n)
  {
    U = runif(1,min = 0,max = 1)
    Y = runif(1,min = 0,max = 1)
    while(U > log(1+Y)/(1+Y))
    {
      U = runif(1,min = 0,max = 1)
      Y = runif(1,min = 0,max = 1)
    }
    X <- c(X, Y)
  }
  return(X)
}

BigF <- function (x)
{
  y = log(1+x)*log(1+x)
  y = y / (log(2)*log(2))
  return (y)
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
  X = c()
  for (i in 1:n)
  {
    U = runif(1,min = 0,max = 1)
    inv = exp(sqrt(U)*log(2))-1
    X <- c(X, inv)
  }
  return (X)
}

LoiBinomiale <- function (n,p,k)
{
  B =  c()
  for (i in 1:k)
  {
    U = runif(n,min = 0,max = 1)
    B <- c(B, sum(U < p))
  }
  
  return (B)
}

BinomialeSj <- function (n,p,j) # Algo du sujet
{
  B = c()
  for (i in 1:j)
  {
    k = -1
    somme = 0
    U = runif(1,min = 0,max = 1)
    while (U > somme)
    {
      k <- k+1
      somme = somme + choose(n,k) * (p^k) * ((1-p)^(n-k))
    }
    B <- c(B, k)
  }
  return (B)
}