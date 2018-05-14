phi1=0.3;miu1=5;sigma1=5
phi2=0.2;miu2=20;sigma2=3
phi3=0.5;miu3=50;sigma3=10

N=10000
x=rep(0,N)
rate=runif(N,0,1)
for (i in 1:N){
  if (rate[i]<=phi1){
    x[i]=rnorm(1,miu1,sigma1)
  }else if (rate[i]<=(phi1+phi2)){
    x[i]=rnorm(1,miu2,sigma2)
  }else{
    x[i]=rnorm(1,miu3,sigma3)
  }
}

hist(x,breaks = 100)

miu=c(0,5,10)
sigma=c(5,5,5)
phi=c(0.33,0.33,0.34)
w=matrix(0,N,3)

T=50
miu_=matrix(0,T+1,3)
sigma_=matrix(0,T+1,3)
phi_=matrix(0,T+1,3)
miu_[1,]=miu
sigma_[1,]=sigma
phi_[1,]=phi

for (t in 1:T){
  for (k in 1:3){
    w[,k]=phi[k]*dnorm(x,miu[k],sigma[k])
  }
  w1=matrix(1,N,3)
  for(i in 1:N){
    w1[i,1]=sum(w[i,])
    w1[i,2]=sum(w[i,])
    w1[i,3]=sum(w[i,])
  }
  w=w/w1
  
  for(k in 1:3){
    miu[k]=w[,k]%*%x/sum(w[,k])
    sigma[k]=(w[,k]%*%((x-miu[k])*(x-miu[k]))/sum(w[,k]))^(1/2)
    phi[k]=sum(w[,k])/N
  }
  miu_[t+1,]=miu
  sigma_[t+1,]=sigma
  phi_[t+1,]=phi
}

plot(phi_[,1],ylab='phi',ylim = c(0,1),col='red',type='l')
points(phi_[,2],col='green',type = 'l')
points(phi_[,3],col='blue',type='l')

plot(miu_[,1],ylab='miu',ylim = c(0,60),col='red',type='l')
points(miu_[,2],col='green',type = 'l')
points(miu_[,3],col='blue',type = 'l')

plot(sigma_[,1],ylab='sigma',ylim=c(0,20),col='red',type = 'l')
points(sigma_[,2],col='green',type = 'l')
points(sigma_[,3],col='blue',type = 'l')

