library(randtoolbox)
source('functions.R')

queue <- FileMM1(8,15,12)
results <-MM1Evolution(queue)
results[[1]]
results[[2]]

plot(results[[1]], results[[2]])
plot(results[[1]], results[[2]], 's')

