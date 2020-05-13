library(randtoolbox)
source('distributions.R')
n = 50
p = 0.5
B = LoiBinomiale(n,p,1000)

par(mfrow=c(1,2))

plot(table(B))

X = seq(from = 1, to = n, by =  1)
plot(dnorm(X,n*p, sqrt(n*p*(1-p))))


n2 = 500
X2 = Inversion(n2)
hist(X2)
X3 = Rejet(n2)
hist(X3)

X_lin = seq(from = 0, to = 1, by =  0.1)
plot(f(X_lin))
plot(BigF(X))

microbenchmark::microbenchmark(times=100,Rejet(1000),Inversion(1000))