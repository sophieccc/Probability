# au prealable, vous devez executer l'instruction suivante
# install.packages('randtoolbox')

library(randtoolbox)
source('generateurs.R')

sVN <- 9721
sMT <- 2504
sR <- 999
sSM <- 999
Nsimu <- 1000
Nrepet <- 20


############################################################
##  Section 2
############################################################

vn <- VonNeumann(Nsimu,Nrepet,sVN)
mt <- MersenneTwister(Nsimu,Nrepet,sMT)
randu = RANDU(Nsimu, sR)
sm = StandardMinimal(Nsimu, sSM)

par(mfrow=c(2,2))
hist(mt[,1],xlab='',main='Mersenne Twister')
hist(vn[,1],xlab='',main='Von Neumann')
hist(randu[,1],xlab='',main='RANDU')
hist(sm[,1],xlab='',main='Standard Minimal')

par(mfrow=c(2,2))
plot(mt[1:(Nsimu-1),1],mt[2:Nsimu,1],xlab='MT(i)', ylab='MT(i+1)', main='Mersenne Twister')
plot(vn[1:(Nsimu-1),1],vn[2:Nsimu,1],xlab='VN(i)', ylab='VN(i+1)', main='Von Neumann')
plot(randu[1:(Nsimu-1),1],randu[2:Nsimu,1],xlab='RANDU(i)', ylab='RANDU(i+1)', main='RANDU')
plot(sm[1:(Nsimu-1),1],sm[2:Nsimu,1],xlab='SM(i)', ylab='SM(i+1)', main='Standard Minimal')

# Sequence de bits pour les tests
(bit_mt <- binary(mt[1,1]))

