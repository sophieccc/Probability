FileMM1 <- function (lambda, mu, D)
{
  arrive = vector()
  depart = vector()
  
  temps <- 0
  k <- 0
  while(time < D)
  {
    time <- time + rexp(1, rate=lambda)
    if (time < D)
    {
      arrive[k] = time
      k <- k+1;
    }
  }
  
  if(length(arrive)>0)
  {
    depart [1] = arrive [1] + rexp(1, rate=mu)
    for (i in 2:length(arrive))
    {
      if (arrive [i] > depart[i-1])
      {
        time = arrive [i] + rexp(1, rate=mu)
      }
      else
      {
        time = depart [i-1] + rexp(1, rate=mu)
      }
      if (time <D)
      {
        depart [i] = time
      }
      else
      {
        break
      }
    }
  }
}