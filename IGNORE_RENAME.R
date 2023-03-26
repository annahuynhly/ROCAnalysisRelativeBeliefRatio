# binormal analysis with equal variances
# 1. The setup

# the hyperparamters for the prior on the mu's and sigma's and w
mu0=0
tau0=0.5
alpha0=1.787
beta0=1.056
a1=15.3589 
a2=22.53835

# the data 
nND=25
xND=-0.072
sND2=19.638

nD=20
xD=0.976
sD2=16.778

#w = rbeta(1000,a1,a2)

# the values of the hyperparameters for the posterior based on the prior and the data 
alpha0post <- alpha0+(nD+nND)/2
tau0D=1/sqrt(nD+1/tau0^2)
tau0ND=1/sqrt(nND+1/tau0^2)
beta0post <- beta0+(sD2+sND2)/2+(tau0D**2)*(nD/tau0^2)*(xD-mu0)^2/2+(tau0ND**2)*(nND/tau0^2)*(xND-mu0)^2/2
mu0Dpost=(tau0D**2)*(nD*xD+mu0/tau0**2)
mu0NDpost=(tau0ND**2)*(nND*xND+mu0/tau0**2)
a1post=a1+nD
a2post=a2+nND

# L= number of subintervals of [0,1] for estimating density of copt 
L<-100
A=rep(0,L+1)
for (i in 1:(L+1)) {
  A[i]=(i-1)/L
}
grid=rep(0,L)
for (i in 1:L) {
  grid[i]=(i-1/2)/L
}

# the integrand in the computation of the AUC
fcnAUC <- function(z){
  return (dnorm(z)*pnorm((muDi-muNDi)/sigmaDi+sigmaNDi/sigmaDi*z))
}


#----------------------------------------------------------------------
#2. Using the conditional prior to make inferences about copt

# getting the prior distrbution of copt

nMonteprior=100000

# samples from (muD,sigmaD,muND,sigmaND) and prevalence w
sigmaD <- sqrt(1/rgamma(nMonteprior,alpha0,beta0))
sigmaND=sigmaD
muD = rnorm(nMonteprior,mu0,(tau0*sigmaD))
priorimpwt=pnorm((muD-mu0)/(tau0*sigmaND))
U=rbeta(nMonteprior,1,1)
muND = mu0+tau0*sigmaND*qnorm(priorimpwt*U)
# prevalence
w = rbeta(nMonteprior,a1,a2)

c=0.5*(muD+muND)+(sigmaD**2)*(log((1-w)/w))/(muD-muND)
cmod=(pi/2+atan(c))/pi
cmodmax=max(cmod)
cmodmin=min(cmod)
cmodmax
cmodmin

priorcmod <- rep(0,L)

for (iMonteprior in 1:nMonteprior) {
  for (igrid in 1:L){
    if ( (A[igrid] < cmod[iMonteprior]) & (cmod[iMonteprior] <= A[igrid+1]) ) {
      priorcmod[igrid]=priorcmod[igrid]+priorimpwt[iMonteprior]
    }
  }
}
priorcmod=priorcmod/sum(priorcmod)
priorcmoddensity=L*priorcmod
plot(grid, priorcmoddensity, xlab="cmod",ylab="prior",type="l",lty=1)


# getting the posterior

nMontepost=100000
sigmaDpost <- sqrt(1/rgamma(nMontepost,alpha0post,beta0post))
sigmaNDpost=sigmaDpost
muDpost = mu0Dpost+tau0D*sigmaDpost*rnorm(nMontepost,0,1)
postimpwt=pnorm((muDpost-mu0NDpost)/(tau0ND*sigmaNDpost))
U=rbeta(nMontepost,1,1)
muNDpost = mu0NDpost+tau0ND*sigmaNDpost*qnorm(postimpwt*U)
wpost= rbeta(nMontepost,a1post,a2post)

c=0.5*(muDpost+muNDpost)+(sigmaDpost**2)*(log((1-wpost)/wpost))/(muDpost-muNDpost)
cmod=(pi/2+atan(c))/pi
cmodmax=max(cmod)
cmodmin=min(cmod)
cmodmax
cmodmin

postcmod <- rep(0,L)

# this is the loop for the Monte Carlo for the posterior
for (iMontepost in 1:nMontepost) { 
  for (igrid in 1:L){
    if ( (A[igrid] < cmod[iMontepost]) & (cmod[iMontepost] <= A[igrid+1]) ) {
      postcmod[igrid]=postcmod[igrid]+postimpwt[iMontepost] 
    }
  }
}

postcmod=postcmod/sum(postcmod)
postcmoddensity=L*postcmod
plot(grid,postcmoddensity,xlab="cmod",ylab="prior and posterior",type="l",lty=1)
lines(grid, priorcmoddensity, type="l",lty=2)

# obtain relative belief ratio and inferences

RBcmod <- postcmod/priorcmod

postPlcmod=0
print("The values in the plausible region for cmod, copt, the RB and the post. prob. at those values")
for (i in 1:L) {
  if (priorcmod[i] > 0 & RBcmod[i] > 1 ) {
    postPlcmod=postPlcmod+postcmod[i]
    cat(grid[i],tan(pi*grid[i]-pi/2),RBcmod[i],postcmod[i],"\n")}
}
cat("The posterior content of the plausible region = ",postPlcmod,"\n")

imax=1

RBcmod[is.na(RBcmod)] = 0 # DENOTE 0 TEMPORARILY
for (i in 1:L) {
  if (priorcmod[i] >0 & RBcmod[i] > RBcmod[imax] ) {imax=i }
}
cmodest=grid[imax]
cmodest
coptest=tan(pi*cmodest-pi/2)
coptest

par(mfrow=c(1,2))
plot(grid,postcmoddensity,xlab="cmod",ylab=expression("prior and posterior"),type="l",lty=1)
lines(grid, priorcmoddensity, type="l",lty=2)
plot(grid,RBcmod,xlab="cmod",ylab=expression("RB"),type="l",lty=1)

#-------------------------------------------------------------------------------------#####################
#3. obtain error characteristics at copt estimate

coptest = 0.715

# prior
nMonteprior= 3000000

# samples from (muD,sigmaD,muND,sigmaND) and prevalence w
sigmaD <- sqrt(1/rgamma(nMonteprior,alpha0,beta0))
sigmaND=sigmaD
muD = rnorm(nMonteprior,mu0,(tau0*sigmaD))
priorimpwt=pnorm((muD-mu0)/(tau0*sigmaND))
U=rbeta(nMonteprior,1,1)
muND = mu0+tau0*sigmaND*qnorm(priorimpwt*U)
# prevalence
w= rbeta(nMonteprior,a1,a2)



FNR = pnorm((coptest-muD)/sigmaD)
FPR = 1-pnorm((coptest-muND)/sigmaND)
Error = w*FNR+(1-w)*FPR
FDR = (1-w)*FPR/((1-w)*FPR+w*(1-FNR))
FNDR = w*FNR/(w*FNR+(1-w)*(1-FPR))

priorFNR <- rep(0,L)
priorFPR <- rep(0,L)
priorError <- rep(0,L)
priorFDR <- rep(0,L)
priorFNDR <- rep(0,L)

for (iMonteprior in 1:nMonteprior) {
  for (igrid in 1:L){
    if ( (A[igrid] < FNR[iMonteprior]) & (FNR[iMonteprior] <= A[igrid+1]) ) {
      priorFNR[igrid]=priorFNR[igrid]+priorimpwt[iMonteprior]}
    if ( (A[igrid] < FPR[iMonteprior]) & (FPR[iMonteprior] <= A[igrid+1]) ) {
      priorFPR[igrid]=priorFPR[igrid]+priorimpwt[iMonteprior]}
    if ( (A[igrid] < Error[iMonteprior]) & (Error[iMonteprior] <= A[igrid+1]) ) {
      priorError[igrid]=priorError[igrid]+priorimpwt[iMonteprior]}
    if ( (A[igrid] < FDR[iMonteprior]) & (FDR[iMonteprior] <= A[igrid+1]) ) {
      priorFDR[igrid]=priorFDR[igrid]+priorimpwt[iMonteprior]}
    if ( (A[igrid] < FNDR[iMonteprior]) & (FNDR[iMonteprior] <= A[igrid+1]) ) {
      priorFNDR[igrid]=priorFNDR[igrid]+priorimpwt[iMonteprior]}
  }
}

priorFNR=priorFNR/sum(priorFNR)
priorFNRdensity=L*priorFNR

priorFPR=priorFPR/sum(priorFPR)
priorFPRdensity=L*priorFPR

priorError=priorError/sum(priorError)
priorErrordensity=L*priorError

priorFDR=priorFDR/sum(priorFDR)
priorFDRdensity=L*priorFDR

priorFNDR=priorFNDR/sum(priorFNDR)
priorFNDRdensity=L*priorFNDR


plot(grid, priorFNRdensity, xlab="FNR",ylab="prior",type="l",lty=1)
plot(grid, priorFPRdensity, xlab="FPR",ylab="prior",type="l",lty=1)
plot(grid, priorErrordensity, xlab="Error",ylab="prior",type="l",lty=1)
plot(grid, priorFDRdensity, xlab="FDR",ylab="prior",type="l",lty=1)
plot(grid, priorFNDRdensity, xlab="FNDR",ylab="prior",type="l",lty=1)


# posterior ########################################################################################
nMontepost=3000000

# samples from (muD,sigmaD,muND,sigmaND) and prevalence w
sigmaDpost <- sqrt(1/rgamma(nMontepost,alpha0post,beta0post))
sigmaNDpost=sigmaDpost
muDpost = rnorm(nMontepost,mu0Dpost,(tau0D*sigmaDpost))
postimpwt=pnorm((muDpost-mu0NDpost)/(tau0ND*sigmaNDpost))
U=rbeta(nMontepost,1,1)
muNDpost = mu0NDpost+tau0ND*sigmaNDpost*qnorm(postimpwt*U)
# prevalence
wpost=rbeta(nMontepost,a1post,a2post)

FNRpost=pnorm((coptest-muDpost)/sigmaDpost)
FPRpost=1-pnorm((coptest-muNDpost)/sigmaNDpost)
Errorpost=wpost*FNRpost+(1-wpost)*FPRpost
FDRpost=(1-wpost)*FPRpost/((1-wpost)*FPRpost+wpost*(1-FNRpost))
FNDRpost=wpost*FNRpost/(wpost*FNRpost+(1-wpost)*(1-FPRpost))

postFNR <- rep(0,L)
postFPR <- rep(0,L)
postError <- rep(0,L)
postFDR <- rep(0,L)
postFNDR <- rep(0,L)

for (iMontepost in 1:nMontepost) {
  for (igrid in 1:L){
    if ( (A[igrid] < FNRpost[iMontepost]) & (FNRpost[iMontepost] <= A[igrid+1]) ) {
      postFNR[igrid]=postFNR[igrid]+postimpwt[iMontepost]}
    if ( (A[igrid] < FPRpost[iMontepost]) & (FPRpost[iMontepost] <= A[igrid+1]) ) {
      postFPR[igrid]=postFPR[igrid]+postimpwt[iMontepost]}
    if ( (A[igrid] < Errorpost[iMontepost]) & (Errorpost[iMontepost] <= A[igrid+1]) ) {
      postError[igrid]=postError[igrid]+postimpwt[iMontepost]}
    if ( (A[igrid] < FDRpost[iMontepost]) & (FDRpost[iMontepost] <= A[igrid+1]) ) {
      postFDR[igrid]=postFDR[igrid]+postimpwt[iMontepost]}
    if ( (A[igrid] < FNDRpost[iMontepost]) & (FNDRpost[iMontepost] <= A[igrid+1]) ) {
      postFNDR[igrid]=postFNDR[igrid]+postimpwt[iMontepost]}
  }
}

postFNR=postFNR/sum(postFNR)
postFPR=postFPR/sum(postFPR)
postError=postError/sum(postError)
postFDR=postFDR/sum(postFDR)
postFNDR=postFNDR/sum(postFNDR)

# obtain relative belief ratio and inferences --------- NEED TO ADD!!!!!!!!!!!! ######################

RBFNR <- postFNR/priorFNR
RBFPR <- postFPR/priorFPR
RBError <- postError/priorError
RBFDR <- postFDR/priorFDR
RBFNDR <- postFNDR/priorFNDR

# to get a starting value for imax
for (i in 1:L) {
  if (priorFNR[i]>0) {imaxFNR=i}
  if (priorFPR[i]>0) {imaxFPR=i}
  if (priorError[i]>0) {imaxError=i}
  if (priorFDR[i]>0) {imaxFDR=i}
  if (priorFNDR[i]>0) {imaxFNDR=i}
}
imaxFNR
imaxFPR
imaxError
imaxFDR
imaxFNDR
for (i in 1:L) {
  if (priorFNR[i] >0 & RBFNR[i] > RBFNR[imaxFNR] ) {imaxFNR=i }
  if (priorFPR[i] >0 & RBFPR[i] > RBFPR[imaxFPR] ) {imaxFPR=i }
  if (priorError[i] >0 & RBError[i] > RBError[imaxError] ) {imaxError=i }
  if (priorFDR[i] >0 & RBFDR[i] > RBFDR[imaxFDR] ) {imaxFDR=i }
  if (priorFNDR[i] >0 & RBFNDR[i] > RBFNDR[imaxFNDR] ) {imaxFNDR=i }
}

FNRest=grid[imaxFNR]
FPRest=grid[imaxFPR]
Errorest=grid[imaxError]
FDRest=grid[imaxFDR]
FNDRest=grid[imaxFNDR]
FNRest
FPRest
Errorest
FDRest
FNDRest

postFNRdensity=L*postFNR
postFPRdensity=L*postFPR
postErrordensity=L*postError
postFDRdensity=L*postFDR
postFNDRdensity=L*postFNDR






par(mfrow=c(1,2))
plot(grid, postFNRdensity, xlab="FNR",ylab="prior and posterior",type="l",lty=1)
lines(grid,priorFNRdensity, xlab="FNR",ylab="prior and posterior",type="l",lty=2)
plot(grid,RBFNR,xlab="FNR",ylab=expression("RB"),type="l",lty=1)

par(mfrow=c(1,2))
plot(grid, postFPRdensity, xlab="FPR",ylab="prior and posterior",type="l",lty=1)
lines(grid,priorFPRdensity, xlab="FNR",ylab="prior and posterior",type="l",lty=2)
plot(grid,RBFPR,xlab="FPR",ylab=expression("RB"),type="l",lty=1)

par(mfrow=c(1,2))
plot(grid, postErrordensity, xlab="Error",ylab="prior and posterior",type="l",lty=1)
lines(grid,priorErrordensity, xlab="Error",ylab="prior and posterior",type="l",lty=2)
plot(grid,RBError,xlab="Error",ylab=expression("RB"),type="l",lty=1)

par(mfrow=c(1,2))
plot(grid, postFDRdensity, xlab="FDR",ylab="prior and posterior",type="l",lty=1)
lines(grid,priorFDRdensity, xlab="FNR",ylab="prior and posterior",type="l",lty=2)
plot(grid,RBFDR,xlab="FDR",ylab=expression("RB"),type="l",lty=1)

par(mfrow=c(1,2))
plot(grid, postFNDRdensity, xlab="FNDR",ylab="prior and posterior",type="l",lty=1)
lines(grid,priorFNDRdensity, xlab="FNDR",ylab="prior and posterior",type="l",lty=2)
plot(grid,RBFNDR,xlab="FNDR",ylab=expression("RB"),type="l",lty=1)



plot(grid,priorFNRdensity, xlab="FNDR",ylab="prior and posterior",type="l",lty=2)




