library(randtoolbox)
source('functions.R')
lambda <- 8
mu <- 15
D <- 720

queue <- FileMM1(lambda,mu, D)
results <-MM1Evolution(queue)
plot(results[[1]],results[[2]],'s')
temps <- AvgTime(queue)
#print(temps)
N <- AvgAttendance(lambda, mu)
#print(N)
print(N/temps)