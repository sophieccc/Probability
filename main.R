library(randtoolbox)
source('distributions.R')
n = 50
p = 0.5
B = LoiBinomiale(n,p,1000)

par(mfrow=c(1,2))

plot(table(B))

X = seq(from = 1, to = n, by =  1)
plot(dnorm(X,n*p, sqrt(n*p*(1-p))))