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
  for(m in c((n+1),n+2,5,9)){
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


###plots for the supplementary material
library(ggplot2)

trunc$ntext<-paste0("n=",trunc$n)
ggplot((trunc),aes(sig0,m1,col=factor(m)))+geom_line(size=2)+facet_grid(~ntext)+
   theme_bw(base_size=20)+xlab(expression(sigma^{2}))+ylab(expression(med(r[paste(n,",",m)])))+ labs(col = "m")


ggplot(subset(trunc,n<3&m %in% c(2,3,5)&UCL %in% c(0.2,0.5,0.8)),aes(sig0,m1t,col=factor(k)))+geom_line(size=1)+facet_grid(UCL~paste0(n,"o",m))+
  geom_hline(yintercept=1, col=1, lty=2)+labs(col = "k")+
  geom_line(aes(sig0,m1),linetype=1,size=1,col=1)+theme_bw(base_size=20)+xlab(expression(sigma^{2}))+ylab(expression(med(r[paste(n,",",m)])))

                  