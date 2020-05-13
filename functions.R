FileMM1 <- function (lambda, mu, D)
{
  arrive <- c()
  depart <- c()
  
  time <- 0
  k <- 0
  while(time < D)
  {
    time <- time + rexp(1, rate=lambda)
    if (time < D)
    {
      arrive <- c(arrive, time)
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
        time <- arrive [i] + rexp(1, rate=mu)
      }
      else
      {
        time <- depart [i-1] + rexp(1, rate=mu)
      }
      if (time < D)
      {
        depart <- c(depart, time)
      }
      else
      {
        break
      }
    }
  }
  queue <- list(arrive, depart)
  return(queue)
}

MM1Evolution <- function(queue)
{
  arrive <- queue[[1]]
  depart <- queue[[2]]
  nbarrive <- length(arrive)
  nbdepart <- length(depart)
  time <- matrix(nrow=nbarrive + nbdepart +1, ncol=1)
  attendees <- matrix(nrow=nbarrive + nbdepart +1, ncol=1)
  time[1] = 0
  attendees[1]= 0
  
  i <- 1
  j <- 1
  while ( i <= nbarrive && j <= nbdepart)
  {
    if (arrive[i] <= depart[j])
    {
      attendees[i+j] = attendees [i+j-1] + 1
      time [i+j] = arrive [i] 
      i <- i+1
    }
    else
    {
      attendees[i+j] = attendees [i+j-1] - 1
      time [i+j] = depart [j] 
      j <- j+1
    }
  }
  if (i <= nbarrive)
  {
    for (k in i:nbarrive)
    {
      attendees[k+j] = attendees[k+j-1] + 1;
      time [k+j] = arrive [k]
    }
  }
  else if (j <= nbdepart)
  {
    for (k in j:nbdepart)
    {
      attendees[k+i] = attendees[k+i-1] - 1;
      time [k+i] = depart [k]
    }
  }
  results <- list(time, attendees)
  return(results)
}

AvgTime <- function(queue)
{
  arrive <- queue[[1]]
  depart <- queue[[2]]
  
  nbarrive <- length(arrive)
  nbdepart <- length(depart)
  avg <- 0 
  for (i in 1:length(depart))
  {
    avg <- avg + depart[i] - arrive [i]
  }
  avg <- avg / length(depart)
  return(avg)
}

AvgAttendance <- function(lambda, mu)
{
  alpha = lambda / mu
  E = (alpha) / (1 - alpha)
  return(E)
}





