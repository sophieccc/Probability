library(randtoolbox)
source('functions.R')

queue <- FileMM1(20,15, 12)
results <-MM1Evolution(queue)
plot(results[[1]],results[[2]],'s')