phi1=0.2;miu1=5;sigma1=5
phi2=0.2;miu2=30;sigma2=3
phi3=0.6;miu3=50;sigma3=8

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

hist(x,col='gray',breaks=100,freq=F)