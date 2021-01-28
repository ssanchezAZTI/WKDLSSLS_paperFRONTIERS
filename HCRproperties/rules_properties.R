###############################################
## Properties of the n-over-m trend-based rules 
## using Fenton-wilkinson approximation
## 15/10/2020 Leire Citores (Azti)
###############################################

#compute the ratio between the average of n lognormally distributed iid vairables 
# and lognormally distributed iid vairables ,(r_n,m), with an dwitoug uncertainty cups.

trunc<-c() #object for outputs
UCL<-c(seq(0.1,0.9,0.1)) #UCL 
k<-c(seq(1,6,1))      #k, multiplier to compute UCU (UCU=k*UCL)
sigma<-c(seq(0.1,3,0.1)) #standar deviation of observed abundance
for (n in 1:3){
  for(m in c((n+1),n+2,5,6)){
print(paste(n,m))

x1 <- exp(1/2 * log( n/m * (exp(sigma^2)-1+m)/(exp(sigma^2)-1+n) ))
sig1 <- sqrt(log((exp(sigma^2)-1)/n +1) + log((exp(sigma^2)-1)/m +1))

for(r_m in k){
for (i in 1:length(x1)){
  x<-x1[i]
  sig<-sig1[i]
  #print(x)
  #for(sig in sig1){
    for(r1 in UCL){
    low<-log(1-r1)
    r2<-r1*r_m #UCU
    up<-log(1+r2)
    logr.sig <-sig
    logr.med<-log(x)
    x1t<-logr.med + qnorm( (pnorm( (low - logr.med)/logr.sig ) + pnorm( (up - logr.med)/logr.sig ))/2 )*logr.sig 
    mult<-(qnorm( (pnorm( (low - logr.med)/logr.sig ) + pnorm( (up - logr.med)/logr.sig ))/2 ) )
    
    ci_l<-exp(logr.med-1*(logr.sig))
    ci_u<-exp(logr.med+1*(logr.sig))
    
    trunc<-rbind(trunc,cbind(x1=x,sig1=sig,UCL=r1,UCU=r2,k=r_m,m1=x,m1t=exp(x1t),n=n,m=m,sig0=sigma[i],mult=mult,ci_u=ci_u,ci_l=ci_l))
    }
  #}
    }
}
  }}
trunc<-as.data.frame(trunc)



ggplot(as.data.frame(trunc),aes(sig0,m1t,col=factor(k)))+geom_line(size=1)+facet_grid(UCL~paste0(n,"o",m))+
geom_hline(yintercept=1, col=1, lty=2)+
  geom_line(aes(sig0,m1),linetype=1,size=1,col=1)+theme_bw()+ylab("med(r_n,m)")+xlab("sigma2")


ggplot(as.data.frame(trunc),aes(sig0,m1,col=paste0("n o ",m)))+geom_line(size=2)+facet_grid(~n)+
  theme_bw()+xlab("sigma2")+ylab("med(r_n,m)")+ labs(col = "n-over-m")

ggplot(as.data.frame(trunc),aes(sig0,m1,col=paste0(n ,"o m")))+geom_line(size=2)+facet_grid(~m)+
  theme_bw()+xlab("sigma2")+ylab("med(r_n,m)")+ labs(col = "n-over-m")

