# au prealable, vous devez executer l'instruction suivante
# install.packages('randtoolbox')

library(randtoolbox)
source('generateurs.R')

sVN <- 9721
sMT <- 2504
Nsimu <- 1000
Nrepet <- 1
grain <- 999


############################################################
##  Section 2
############################################################

vn <- VonNeumann(Nsimu,Nrepet,sVN)
mt <- MersenneTwister(Nsimu,Nrepet,sMT)
ru <- RANDU(Nsimu, grain)
sm <- STANDARD_MINI(Nsimu, grain)

par(mfrow=c(1,2))
hist(mt[,1],xlab='',main='Mersenne Twister')
hist(vn[,1],xlab='',main='Von Neumann')
hist(ru[,1],xlab='',main='RANDU')
hist(sm[,1],xlab='',main='Standard Minimal')


par(mfrow=c(1,2))
plot(mt[1:(Nsimu-1),1],mt[2:Nsimu,1],xlab='MT(i)', ylab='MT(i+1)', main='Mersenne Twister')
plot(vn[1:(Nsimu-1),1],vn[2:Nsimu,1],xlab='VN(i)', ylab='VN(i+1)', main='Von Neumann')
plot(ru[1:(Nsimu-1),1],ru[2:Nsimu,1],xlab='RU(i)', ylab='RU(i+1)', main='RANDU')
plot(sm[1:(Nsimu-1),1],sm[2:Nsimu,1],xlab='SM(i)', ylab='SM(i+1)', main='Standard Minimal')

# Sequence de bits pour les tests
#(bit_mt <- binary(mt[1,1]))

# Test de fréquence monobit
samples = sample.int(100000,100)

PVMT_F = matrix(nrow=length(samples), ncol=1)
PVRandU_F = matrix(nrow=length(samples), ncol=1)
PVSM_F = matrix(nrow=length(samples), ncol=1)
PVVnM_F = matrix(nrow=length(samples), ncol=1)

for(i in 1:length(samples))
{
  vn <- VonNeumann(Nsimu,Nrepet,samples[i])
  mt <- MersenneTwister(Nsimu,Nrepet,samples[i])
  randu = RANDU(Nsimu, samples[i])
  sm = STANDARD_MINI(Nsimu, samples[i]) 
  
  PVMT_F [i] = Frequency(mt, 32)
  PVRandU_F [i] = Frequency(randu, 31)
  PVSM_F [i] = Frequency(sm,31)
  PVVnM_F [i] = Frequency(vn, 14)
}


par(mfrow=c(2,2))
plot(PVMT_F)
plot(PVVnM_F)
plot(PVRandU_F)
plot(PVSM_F)



vnP_F = mean(PVVnM_F)
mtP_F = mean(PVMT_F)
randuP_F = mean(PVRandU_F)
smP_F = mean(PVSM_F)

vnSD_F = sd(PVVnM_F)
mtSD_F = sd(PVMT_F)
rdSD_F = sd (PVRandU_F)
smSD_F = sd(PVSM_F)

vnV_F = var(PVVnM_F)
mtV_F = var(PVMT_F)
randuV_F = var (PVRandU_F)
smV_F = var (PVSM_F)

print(vnV_F)
print(mtV_F)
print(randuV_F)
print(smV_F)

# Test des Runs

samples = sample.int(100000,100)

PVMT_R = matrix(nrow=length(samples), ncol=1)
PVRandU_R = matrix(nrow=length(samples), ncol=1)
PVSM_R = matrix(nrow=length(samples), ncol=1)
PVVnM_R = matrix(nrow=length(samples), ncol=1)

for(i in 1:length(samples))
{
  vn <- VonNeumann(Nsimu,Nrepet,samples[i])
  mt <- MersenneTwister(Nsimu,Nrepet,samples[i])
  randu = RANDU(Nsimu, samples[i])
  sm = STANDARD_MINI(Nsimu, samples[i]) 
  
  PVMT_R [i] = Runs(mt, 32)
  PVRandU_R [i] = Runs(randu, 31)
  PVSM_R [i] = Runs(sm,31)
  PVVnM_R [i] = Runs(vn, 14)
}


par(mfrow=c(1,2))
plot(PVMT_R)
plot(PVVnM_R)
plot(PVRandU_R)
plot(PVSM_R)



vnP_R = mean(PVVnM_R)
mtP_R = mean(PVMT_R)
randuP_R = mean(PVRandU_R)
smP_R = mean(PVSM_R)

vnSD_R = sd(PVVnM_R)
mtSD_R = sd(PVMT_R)
rdSD_R = sd (PVRandU_R)
smSD_R = sd(PVSM_R)

vnV_R = var(PVVnM_R)
mtV_R = var(PVMT_R)
randuV_R = var (PVRandU_R)
smV_R = var (PVSM_R)

print(vnV_R)
print(mtV_R)
print(randuV_R)
print(smV_R)
# RandU réussit moins bien le test des runs que celui de fréq monobit

# Test d'ordre
samples = sample.int(100000,100)

PVMT_O = matrix(nrow=length(samples), ncol=1)
PVRandU_O = matrix(nrow=length(samples), ncol=1)
PVSM_O = matrix(nrow=length(samples), ncol=1)
PVVnM_O = matrix(nrow=length(samples), ncol=1)

for(i in 1:length(samples))
{
  vn <- VonNeumann(Nsimu,Nrepet,samples[i])
  mt <- MersenneTwister(Nsimu,Nrepet,samples[i])
  randu = RANDU(Nsimu, samples[i])
  sm = STANDARD_MINI(Nsimu, samples[i]) 
  
  PVMT_O [i] = randtoolbox::order.test(mt[,1], d=4, echo=FALSE)$p.value
  PVRandU_O [i] = randtoolbox::order.test(randu[,1], d=4, echo=FALSE)$p.value
  PVSM_O [i] = randtoolbox::order.test(sm[,1], d=4, echo=FALSE)$p.value
  PVVnM_O [i] = randtoolbox::order.test(vn[,1], d=4, echo=FALSE)$p.value
}


par(mfrow=c(1,2))
plot(PVMT_O)
plot(PVVnM_O)
plot(PVRandU_O)
plot(PVSM_O)



vnP_O = mean(PVVnM_O)
mtP_O = mean(PVMT_O)
randuP_O = mean(PVRandU_O)
smP_O = mean(PVSM_O)

vnSD_O = sd(PVVnM_O)
mtSD_O = sd(PVMT_O)
rdSD_O = sd (PVRandU_O)
smSD_O = sd(PVSM_O)

vnV_O = var(PVVnM_O)
mtV_O = var(PVMT_O)
randuV_O = var (PVRandU_O)
smV_O = var (PVSM_O)

print(vnV_O)
print(mtV_O)
print(randuV_O)
print(smV_O)

###################################################################
##  Section 3 : Simulations de lois de probabilités quelconques  ##
###################################################################
samples = sample.int(100000,1000)
X = matrix(nrow=1000, ncol=1)
dens = matrix(nrow=1000, ncol=1)
gauss = matrix(nrow=1000, ncol=1)
k=seq(from = 1, to = 1000, by = 1)
p=0.4
for (i in 1:length(X))
{
  n=samples[i]
  k=k[i]
  X[i] = LoiBinomiale(n,p)
  dens [i] = choose(n, k)*(p^k)*((1-p)^(n-k))
  gauss [i] = dnorm(n*p,sqrt(n*p*(1-p)))
}
hist(X[,1],xlab='',main='Binomial Distribution')

hist(abs(dens[,1]-gauss[,1]),xlab='',main='Density')

# uncomment once to install package
# install.packages('microbenchmark')
microbenchmark::microbenchmark(times=100,Rejet(1000),Inversion(1000))
