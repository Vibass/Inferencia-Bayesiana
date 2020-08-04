library(coda)
library(rstan)
library(xlsx)
library(ggplot2)
rstan_options(auto_write=T)
options(mc.cores=parallel::detectCores())
X=read.xlsx("C:/Users/David/Dropbox/TESIS/TABLAS PARA R/hombre_mujer.xlsx",
            sheetIndex = 5)
#X=X[1:40,]
View(X)
N=length(X[,1])
ni=c(X[,4])
xi=c(X[,5])
ti=c(X[,6])
Ti=c(X[,7])
unomenosxi=c(X[,8])
fit=stan(file="C:/Users/David/Dropbox/TESIS/TABLAS PARA R/hombre_mujer.stan",
               data=c("N","ni","xi","ti","Ti","unomenosxi"),iter=2000,chain=4)
fit

#####################
#  SUMARIO
####################
sumario.fit=summary(fit, probs = c(0.025,0.25,0.5,0.75,0.975))$summary
write.xlsx(sumario.fit,"C:/Users/David/Dropbox/TESIS/TABLAS PARA R/sumario2x2.xlsx")
###########################
#  HISTOGRAMAS CUMBAYA
#########################
resultados=extract(fit)
View(resultados)
resultados=as.data.frame(resultados)
medias=apply(resultados,2,mean)
sd=apply(resultados,2,sd)
par(mfrow=c(1,2))
par(mar=c(5.1, 5.1, 8.1, 5.1), xpd=TRUE)
#beta b
hist(resultados[,1000],probability = T,
     main="'beta b'",
     ylab="Densidad",xlab='mean=0.17     sd=0.11',col='deepskyblue4')
#lines(density(resultados[,1000]),col='firebrick1',lwd=2)
mean(resultados[,1000])#corresponde al 1001 en la tabla excel de resultados
sd(resultados[,1000])
#betaw 2220
hist(resultados[,2219],probability = T,
     main="'beta w'",
     ylab="Densidad",xlab='mean=0.54     sd=0.10',col='deepskyblue4')
#lines(density(resultados[,2219]),col='firebrick1',lwd=2)
mean(resultados[,2219])
sd(resultados[,2219])
title(main="\n \n Histogramas parroquia: Cumbayá",outer = TRUE)
########################################
#   CODIGOS DE STAN
########################################
traceplot(fit,pars = c("betab[20]", "betaw[20]"),inc_warmup=TRUE,nrow=2)

stan_hist(fit,pars = c("betaw[1000]","betab[1000]"),inc_warmup=TRUE,main='Cumbaya')

stan_plot(fit,pars = c("betaw[1000]","betab[1000]"),show_density=T, unconstrain = FALSE,
          ci_level=0.8,outer_level=0.95,show_outer_line=TRUE)

stan_ac(fit,pars = c("betaw[1000]","betab[1000]"), include = TRUE, unconstrain = FALSE,
        inc_warmup = FALSE, nrow = NULL, ncol = NULL, 
        separate_chains = TRUE, lags = 30)

stan_scat(fit, unconstrain = FALSE,
          inc_warmup = FALSE, nrow = NULL, ncol = NULL)

stan_dens(fit,pars=c("betaw[1000]","betab[1000]"),include = TRUE, unconstrain = FALSE,
          inc_warmup=FALSE,nrow=NULL,ncol=NULL,separate_chains=TRUE)

stan_trace(fit,pars = c("betaw[1000]","betab[1000]"),include = TRUE, unconstrain = FALSE,
           inc_warmup = TRUE, nrow = NULL, ncol = NULL, window = NULL)

#todas las cadenas
sampler_params=get_sampler_params(fit, inc_warmup = TRUE)
summary(do.call(rbind, sampler_params), digits = 2)
#cadenas separadas
lapply(sampler_params, summary, digits = 2)
#PAIRS
pairs(fit,pars=c("betab[1000]","betaw[1000]","lp__"),las=1)

###################################################
#     plot de los betas en mapa ecuador
###################################################
library(maps)
library(sp)

ecuador=readRDS("/Users/DAVID/Dropbox/TESIS/mapa ecuador/ECU_adm3.rds") 
colors=colorRampPalette(c("blue", "green"))(7)
betasb=medias[1:1219]
betasw=medias[1220:2438]
gendergap=betasw-betasb
min(gendergap)
max(gendergap)
gendergap=as.data.frame(gendergap)
gendergap$ID=seq.int(nrow(gendergap))
gendergap$colorBuckets=as.numeric(cut(gendergap$gendergap,
            c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7)))
colorsmatched=gendergap$colorBuckets[match(ecuador$ID_3,gendergap$ID)]
par(mfrow=c(1,1))
par(mar=c(0,5,3,0), xpd=TRUE)
plot(ecuador,col = colors[colorsmatched],border=F,
     main="Brecha de Género \nDiferencia entre mujeres y hombres",
     cex.main=1.1,xlim=c(-79.5,-77))
#ylim=c(-2.5,2.5)
leg.txt=c("0% a 10%","10% a 20%","20% a 30%","30% a 40%","40% a 50%",
          "50% a 60%","60% a 70%")
legend("left",leg.txt,inset =0,fill=colors,cex=0.7,box.lty=0)

#####################################
#  MAPA DOS - SOLO BETAS B HOMBRES
#####################################
betasb=medias[1:1219]
betasw=medias[1220:2438]
betasb=as.data.frame(betasb)
colorsb=colorRampPalette(c("blue", "green"))(7)
min(betasb)
max(betasb)
betasb$ID=seq.int(nrow(betasb))
betasb$colorBuckets=as.numeric(cut(betasb$betasb,
        c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7)))
colorsmatched=betasb$colorBuckets[match(ecuador$ID_3,betasb$ID)]
par(mfrow=c(1,1))
par(mar=c(0,5,3,0), xpd=TRUE)
plot(ecuador,col = colorsb[colorsmatched],border=F,
     main="Proporción de hombres \nque apoyaron a A. País",
     cex.main=1.1,xlim=c(-79.5,-77))
#ylim=c(-2.5,2.5)
leg.txt=c("0% a 10%","10% a 20%","20% a 30%","30% a 40%","40% a 50%",
          "50% a 60%","60% a 70%")
legend("left",leg.txt,inset=0,fill=colorsb,cex=0.7,box.lty=0)

###########################################
#   MAPA TRES - SOLO BETAS W MUJERES
##########################################
colorsb=colorRampPalette(c("blue", "green"))(10)
betasw=as.data.frame(betasw)
min(betasw)
max(betasw)
betasw$ID=seq.int(nrow(betasw))
betasw$colorBuckets=as.numeric(cut(betasw$betasw,
    c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)))
colorsmatched=betasw$colorBuckets[match(ecuador$ID_3,betasw$ID)]
par(mfrow=c(1,1))
par(mar=c(0,5,3,0), xpd=TRUE)
plot(ecuador,col = colorsb[colorsmatched],border=F,
     main="Proporción de mujeres \nque apoyaron a A. Pais",
     cex.main=1.1,xlim=c(-79.5,-77))
#ylim=c(-2.5,2.5)
leg.txt=c("0% a 10%","10% a 20%","20% a 30%","30% a 40%","40% a 50%",
          "50% a 60%","60% a 70%","70% a 80%","80% a 90%","90% a 100%")
legend("left",leg.txt,inset =0,fill=colorsb,cex=0.8,box.lty=0)

##########################################
#     PLOT xi Vs ti - TOMOGRAFIA
########################################
par(mfrow=c(1,2))
par(mar=c(5.1, 4.1, 4.1, 2.1), xpd=FALSE)
plot(xi,ti,xlim=c(0.3,0.7),ylim=c(0,1),pch='.',cex=2,xlab='Xi', ylab='Ti',
     main='Diagrama de Dispersión')
plot(1,type='n',ylab=expression(paste(beta^w)),xlab=expression(paste(beta^b)),ylim=c(0,1),xlim=c(0,1),
     main="Tomografía")
for (i in 1:length(xi)){
  curve((-xi[i]/unomenosxi[i])*x+ti[i]/unomenosxi[i],ylim=c(0,1),xlim=c(0,1),add=TRUE, 
        ylab='Beta W',xlab='Beta b',col='slategray')
}
#############################################
#      TOMOGRAFIA INTERVALOS DE CONFIANZA
#############################################
sumario.fit=read.xlsx("C:/Users/David/Dropbox/TESIS/IMAGENES/2x2 sin Z/sumariohombre_mujer.xlsx",
            sheetIndex=1)
View(sumario.fit)
CIbetab=cbind(sumario.fit[1:1219,5],sumario.fit[1:1219,9])
CIbetaw=cbind(sumario.fit[1220:2438,5],sumario.fit[1220:2438,9])
betasb=sumario.fit[1:1219,2]
betasw=sumario.fit[1220:2438,2]

#sumario.fit=summary(fit, probs = c(0.1, 0.9))$summary
#CIbetab=sumario.fit[1:1219,4:5]
#CIbetaw=sumario.fit[1220:2438,4:5]
par(mfrow=c(1,1))
par(mar=c(5.1, 4.5, 4.1, 2.1), xpd=FALSE)
plot(1,type='n',ylab=expression(paste(beta^w)),xlab=expression(paste(beta^b)),ylim=c(0,1),xlim=c(0,1),
     main="Tomografía - CI 95%")
for (i in 1:length(xi)){
  curve((-xi[i]/unomenosxi[i])*x+ti[i]/unomenosxi[i],ylim=c(0,1),xlim=c(0,1),add=TRUE, 
        ylab='Beta W',xlab='Beta b',col='white')
}
for (i in 1:length(CIbetab[,1])){
  segments(CIbetab[i,2],CIbetaw[i,1],CIbetab[i,1],
           CIbetaw[i,2],col='slategray')
}
betasbw=as.data.frame(cbind(betasb,betasw))
points(betasbw$betasb,betasbw$betasw,col='red',pch=20,cex=0.5)


##############################################
#   AJUSTE CON VALORES VERDADEROS
#############################################
#sumario.fit=read.xlsx("C:/Users/David/Dropbox/TESIS/IMAGENES/2x2 sin Z/sumariohombre_mujer.xlsx",
#                      sheetIndex=1)
Y=read.xlsx("C:/Users/David/Dropbox/TESIS/TABLAS PARA R/hombre_mujer.xlsx",
            sheetIndex = 4)
Y=Y[1:1219,]
View(Y)
sumario=read.xlsx("C:/Users/David/Dropbox/TESIS/IMAGENES/2x2 sin Z/sumariohombre_mujer.xlsx",
                  sheetIndex = 1)
View(sumario)
realbetab=Y[,11]
realbetaw=Y[,12]
#realbetaw=Y[,12]/(Y[,4])
#sumario=as.data.frame(sumariohombre_mujer)
betab=sumario[1:1219,2]
betaw=sumario[1220:2438,2]
par(mfrow=c(1,1),mar=c(4,4,2, 1),xpd=FALSE,
    oma=c(1,0,2,1))
plot(realbetaw,betaw,xlim=c(0,1),ylim=c(0,1),main='Ajuste',
     xlab=expression(paste('Valores reales:', ~~beta^b,",",~~beta^w)),ylab='Valores inferidos')
points(realbetab,betab,col='blue')
eq = function(x){x}
curve(eq,type='l',add=T,lwd=1.8)
#text(0,0,expression(paste()))
legend('topleft', legend=c(expression(beta^w),expression(beta^b)),
       col=c("black", "blue"), pch=16:16, cex=1,inset=0.02,bty='n')
#CUMBAYA
points(betaw[1000],realbetaw[1000],col='red',pch=19)
points(betab[1000],realbetab[1000],col='red',pch=19)

###############################################
#      PLOT CORRELACION 
###############################################
#sumario=as.data.frame(sumariohombre_mujer)
#X=as.data.frame(h_m) h_m esta en el escritorio
#sumario=as.data.frame(read_excel("C:/Users/David/Dropbox/TESIS/IMAGENES/2x2 sin Z/sumariohombre_mujer.xlsx"))
#X=read_excel("C:/Users/David/Desktop/h-m.xlsx")

#xi=c(X[,5])
betab=sumario[1:1219,2]
betaw=sumario[1220:2438,2]
par(mfrow=c(1,2),mar=c(4,5,2, 1),xpd=FALSE,
    oma=c(1,0,2,1))
plot(xi,betab,pch='.',cex=2,ylab=expression(paste(beta^b)),
     xlab='Xi',main='')
abline(lm(betab~xi),col="red") # regression line (y~x)
plot(unomenosxi,betaw,pch='.',cex=2,ylab=expression(paste(beta^w)),
     xlab='1-Xi',main='')
abline(lm(betaw~unomenosxi),col="red") # regression line (y~x)
title('Correlación',outer = T)
##########################################################                                                        #
#     DESVIACIONES ESTANDARD DE BETA B Y BETA W          #                                                       #
##########################################################
# para BETA b
par(mfrow=c(2,1),mar=c(1,4,1,1)+0.1,xpd=F,oma=c(1,1,2,1))

sdb=sumario[1:1219,4]
xb=seq(1,1219,1)
y0=betab-sdb
y1=betab+sdb
plot(xb,betab,type='n',ylab='sd - beta b',xlab='')
for (i in 1:1219){
  segments(xb[i],y0[i],xb[i],y1[i],col='slategray')
}
points(xb,betab,pch=20,cex=0.8,col='red')
# para BETA W
sdw=sumario[1220:2438,4]
xw=seq(1,1219,1)
betaw=sumario[1220:2438,2]
y0w=betaw-sdw
y1w=betaw+sdw
plot(xw,betaw,type='n',xlab='Observaciones',ylab='sd - beta w')
for (i in 1:1219){
  segments(xw[i],y0w[i],xw[i],y1w[i],col='slategray')
}
points(xw,betaw,pch=20,cex=0.8,col='red')
title("Desviación Estándard", outer=TRUE)
