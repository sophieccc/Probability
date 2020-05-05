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

par(mfrow=c(1,2))
hist(mt[,1],xlab='',main='Mersenne Twister')
hist(vn[,1],xlab='',main='Von Neumann')
hist(randu[,1],xlab='',main='RANDU')
hist(sm[,1],xlab='',main='Standard Minimal')

par(mfrow=c(1,2))
plot(mt[1:(Nsimu-1),1],mt[2:Nsimu,1],xlab='MT(i)', ylab='MT(i+1)', main='Mersenne Twister')
plot(vn[1:(Nsimu-1),1],vn[2:Nsimu,1],xlab='VN(i)', ylab='VN(i+1)', main='Von Neumann')
plot(randu[1:(Nsimu-1),1],randu[2:Nsimu,1],xlab='RANDU(i)', ylab='RANDU(i+1)', main='RANDU')
plot(sm[1:(Nsimu-1),1],sm[2:Nsimu,1],xlab='SM(i)', ylab='SM(i+1)', main='Standard Minimal')

# Sequence de bits pour les tests
(bit_mt <- binary(mt[1,1]))

samples = sample.int(100000,100)
vnP = 0
mtP = 0;
randuP = 0
smP = 0
for(sam in length(samples))
{
  vn <- VonNeumann(Nsimu,Nrepet,sam)
  mt <- MersenneTwister(Nsimu,Nrepet,sam)
  # randu = RANDU(Nsimu, sam)
  # sm = StandardMinimal(Nsimu, sam) 
  
  vnP = vnP + frequency(vn, 14)
  mtP = mtP + frequency(mt, 32)
  randuP = randuP + frequency(randu, 31)
  smP = smP + frequency(sm,31)
}
vnP = vnP / 100
mtP = mtP / 100
randuP = randuP / 100
smP = smP / 100
