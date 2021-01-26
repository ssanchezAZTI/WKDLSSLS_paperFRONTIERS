
# type of rule

n <- 1
m <- 2

sigma <- seq(0,4,by=0.1)
# sigma <- 0.5

# ratio approximated by a lognormal distribution with: 

logr.med <- 1/2 * log( n/m * (exp(sigma^2)-1+m)/(exp(sigma^2)-1+n) )
logr.sig <- sqrt(log((exp(sigma^2)-1)/n +1) + log((exp(sigma^2)-1)/n +1))

# lower and upper uncertainty caps

ucl <- 0.8
ucu <- 4.75

low <- log(1-ucl)
up <- log(1+ucu)

logr.medtr <- logr.med + qnorm( (pnorm( (low - logr.med)/logr.sig ) + pnorm( (up - logr.med)/logr.sig ))/2 )*logr.sig 

plot (sigma, logr.med, type="l")
lines(sigma, logr.medtr, col=2)

plot (sigma, exp(logr.med), type="l")
lines(sigma, exp(logr.medtr), col=2)


uc.vec <- data.frame(ucl=c(0.2, 0.2, 0.5, 0.5, 0.5, 0.8, 0.8, 0.8, 0.8),
                     ucu=c(0.2, 1.25, 0.5, 1, 2, 0.8, 2, 3.5, 5))


# aux <- logr.med
# plot(sigma, exp(aux), type="l", col=1, lty=2, ylim=c(0.5,1.5))
# 
#                      
# for (i in 1:nrow(uc.vec)){
#   low <- log(1-uc.vec$ucl[i])
#   up <- log(1+uc.vec$ucu[i])
#   logr.medtr <- logr.med + qnorm( (pnorm( (low - logr.med)/logr.sig ) + pnorm( (up - logr.med)/logr.sig ))/2 )*logr.sig 
#   aux <- cbind(aux, logr.medtr)
#   lines(sigma, exp(logr.medtr), col=i)
# }



aux <- data.frame(uc="None", sigma=sigma, med=logr.med)

for (i in 1:nrow(uc.vec)){
  low <- log(1-uc.vec$ucl[i])
  up <- log(1+uc.vec$ucu[i])
  logr.medtr <- logr.med + qnorm( (pnorm( (low - logr.med)/logr.sig ) + pnorm( (up - logr.med)/logr.sig ))/2 )*logr.sig 
  aux <- rbind(aux, data.frame(uc=paste(uc.vec$ucl[i],uc.vec$ucu[i],sep="_"), sigma=sigma, med=logr.medtr))
  lines(sigma, exp(logr.medtr), col=i)
}

library(ggplot2)
theme_set(theme_bw())

ggplot(aux, aes(sigma, med, group=uc, col=uc))+
  geom_line(lwd=1)+
  ylim(c(-0.6, 0.4))+
  geom_hline(yintercept=0, col=1, lty=2)
  
ggplot(subset(aux, uc %in% c("None","0.2_0.2","0.5_0.5","0.8_0.8")), aes(sigma, med, group=uc, col=uc))+
  geom_line(lwd=1)+
  ylim(c(-0.6, 0.4))

ggplot(subset(aux, uc %in% c("None","0.2_0.2","0.2_1.25")), aes(sigma, med, group=uc, col=uc))+
  geom_line(lwd=1)+
  ylim(c(-0.6, 0.4))

ggplot(subset(aux, uc %in% c("None","0.5_0.5","0.5_1", "0.5_2")), aes(sigma, med, group=uc, col=uc))+
  geom_line(lwd=1)+
  ylim(c(-0.6, 0.4))

ggplot(subset(aux, uc %in% c("None","0.8_0.8","0.8_2", "0.8_3.5","0.8_5")), aes(sigma, med, group=uc, col=uc))+
  geom_line(lwd=1)+
  ylim(c(-0.6, 0.4))
