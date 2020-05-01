#git add Probabilities.R #git commit -m "Navn på ændring" #git push #git pull
rm(list=ls())
library(matlib) #Til vektor/matrix regning
library(MASS) # Til invers af numerisk problematiske matricer
library(blockmatrix) #matrixopsætning
library(tidyr) #Til data transformation
library(xtable) #Til Latex table
install.packages("C:/Users/Victo/Documents/GitHub/Probability/BTSoccer",
                 repos = NULL,
                 type = "source")
library(BTSoccer)
library(ggplot2)
setwd("C:/Users/Victo/Desktop/bachelor/kode")
setwd("C:/Users/lucas/Desktop/Odd")
#Henter og verificerer data
data <- read.table("Kampe_r1.csv",header=T,sep=",")
data$H <- as.character(data$H)
data$U <- as.character(data$U)
data$dato <- as.Date(data$dato, format = "%m/%d/%Y")
m <- CreateMatrixes(data,"2015-07-17","2016-05-29",33,TRUE)
x <- m$DesignMatrix;Y <- m$KontingensTabel; r <- m$SamledeKampe;
f <- BTFunktioner(x=x,Y=Y,r=r)
#x <- lapply(x[-1,], function(y) y/sum(y))
as.data.frame(x)
prop.tablet(t(x)/rowSums(x))
prop.table(as.matrix(x),1)
as.data.frame.matrix(prop.table(as.matrix(x),1))

for (runde in 2:33){
m <- CreateMatrixes(data,"2015-07-17","2016-05-29",runde,TRUE)
x <- m$DesignMatrix;Y <- m$KontingensTabel; r <- m$SamledeKampe;
f <- BTFunktioner(x=x,Y=Y,r=r)
  if (runde == 2) {
#   funk <- list(f$loglike(beta,theta,x),f$dlbeta(beta,theta,x),f$dltheta(beta,theta,x),f$dl2xtheta(beta,theta,x),f$dlbetatheta(beta,theta,x))
    t1 <- f$loglike(beta,theta,x)
    t2 <- f$dlbeta(beta,theta,x)
    t3 <- f$dltheta(beta,theta,x)
    t4 <- f$dl2xtheta(beta,theta,x)
    t5 <- f$dlbetatheta(beta,theta,x)
    t6 <- f$dl2xbeta(beta,theta,x)
  } else {
    t1 <- t1 + f$loglike(beta,theta,x)
    t2 <- t2 + f$dlbeta(beta,theta,x)
    t3 <- t3 + f$dltheta(beta,theta,x)
    t4 <- t4 + f$dl2xtheta(beta,theta,x)
    t5 <- t5 + f$dlbetatheta(beta,theta,x)
    t6 <- t6 + f$dl2xbeta(beta,theta,x)
  }
}
styrker = 0;y = 0;R=0;X=0;FF=0;
for (runde in 3:33){
  m <- CreateMatrixes(data,"2015-07-17","2016-05-29",runde,TRUE)
  x <- m$DesignMatrix;Y <- m$KontingensTabel; r <- m$SamledeKampe;f <- BTFunktioner(x=x,Y=Y,r=r);
  styrker <- styrker+(exp(t(x)%*%beta));
  toprint <- exp(t(x)%*%beta)/(min(exp(t(x)%*%beta)))
  #print(toprint)
  FF <- FF + f$loglike(beta,theta,x)
  R <- R + r
  y <- y + Y
  X <- X + x
}

exp(t(x[2:12,])%*%beta[2:12])/min(exp(t(x[2:12,])%*%beta[2:12]))
styrker/min(styrker)
beta <- c(rep(0,length(x[1,])))
n <- NR(x)
n$styrker/min(n$styrker)
beta <- n$beta;
theta <- n$theta
n$sd
data1 <- data[which((data$dato>="2015-07-17") & (data$dato<="2016-05-29")),]
UnikHold <- unique(data1$H)
UnikHold <- sort(UnikHold)
runde = 3
#Sandsyligheder vs udfald
m <- CreateMatrixes(data,"2015-07-17","2016-05-29",3,TRUE)

a = 0;b=0;aa=0;bb=0;
aa <- as.matrix(rbind(c(0,0,0)));bb <- as.matrix(rbind(c(0,0,0)));cc <- as.matrix(rbind(c(0,0,0,"H","U")));
for (runde in 3:33){
  m <- CreateMatrixes(data,"2015-07-17","2016-05-29",runde,TRUE)
  x <- m$DesignMatrix;Y <- m$KontingensTabel; r <- m$SamledeKampe;f <- BTFunktioner(x=x,Y=Y,r=r);
  data1 <- data[which((data$dato>="2015-07-17") & (data$dato<="2016-05-29") & (data$runde == runde)),]
  data1$HU <- paste(data1$H,data1$U)
  styrker <- exp(t(x)%*%beta)
  counter = 1;KumSSH <- matrix(NA, nrow = (factorial(length(UnikHold))/(factorial((length(UnikHold)-2)))), ncol = 5)
  for (hold1 in 1:length(UnikHold)) {
    for (hold2 in 1:length(UnikHold)) {
      if (hold1 != hold2){
        KumSSH[counter,] <- cbind(Sandsynligheder(theta,x,styrker,hold1,hold2)[1],Sandsynligheder(theta,x,styrker,hold1,hold2)[2],Sandsynligheder(theta,x,styrker,hold1,hold2)[3],UnikHold[hold1],UnikHold[hold2])
        counter = counter +1
      }
    }
  }
  KumSSH <- as.data.frame.matrix(KumSSH)
  names(KumSSH) <- c("H","U","Uafgjort","H1","H2")
  KumSSH$H1H2 <- paste(KumSSH$H1,KumSSH$H2)
  ssh <- KumSSH[KumSSH$H1H2 %in% data1$HU,]
  ssh$H <- as.numeric(as.character(ssh$H));ssh$U <- as.numeric(as.character(ssh$U));ssh$Uafgjort <- as.numeric(as.character(ssh$Uafgjort))
  ssh <- ssh[order(ssh$H1H2),]
  data1 <- data1[order(data1$HU),]
  as.data.frame(lapply((cbind(data1$Hsejr,data1$Usejr,data1$Uafgjort)-cbind(ssh$H,ssh$U,ssh$Uafgjort))^2, function(y) sum(y)))
  A <- c(rowSums(as.matrix((cbind(data1$Hsejr,data1$Usejr,data1$Uafgjort)-cbind(ssh$H,ssh$U,ssh$Uafgjort))^2)))
  a <- c(append(a,A,after= length(a)))
  B <- c(rowSums((cbind(ssh$H,ssh$U,ssh$Uafgjort)^2)))
  b <- c(append(b,B,after = length(b)))
  AA <- (as.matrix((cbind(data1$Hsejr,data1$Usejr,data1$Uafgjort)-cbind(ssh$H,ssh$U,ssh$Uafgjort))))
  aa <- rbind(aa,AA)
  BB <- as.matrix(((cbind(ssh$H,ssh$U,ssh$Uafgjort))))
  bb <- rbind(bb,BB)
  CC <- as.matrix(((cbind(ssh$H,ssh$U,ssh$Uafgjort,data1$H,data1$U))))
  cc <- rbind(cc,CC)
}
cc <- cc[2:length(cc[,1]),]
sump <- aggregate(as.numeric(cc[,1]),by = list(UnikHold=cc[,4]),FUN = sum)[,2]*3+aggregate(as.numeric(cc[,2]),by = list(UnikHold=cc[,5]),FUN = sum)[,2]*3+
aggregate(as.numeric(cc[,3]),by = list(UnikHold=cc[,4]),FUN = sum)[,2]+aggregate(as.numeric(cc[,3]),by = list(UnikHold=cc[,5]),FUN = sum)[,2]
names(sump) <- names(x)

a <- a[2:length(a)]
b <- b[2:length(b)]
aa <- aa[2:length(aa[,1]),1:3]
bb <- bb[2:length(bb[,1]),1:3]
mean(data1$Uafgjort)

aa <- as.data.frame(aa)
bb <- as.data.frame(bb)
#PLOTS TJEK S: 46, plot ssh. for sejr ift. om man er hjemme eller ude, confusion matrix (antal udfald est. vs udfald)
par(mfrow=c(3,1), mar=c(3,5,3,3))
plot(sort(a))
plot(sort(b))
plot(b,a)
cbind(sort(b),a)
lines(ppoints(a))
plot((sort(aa[,1])))
lines(bb[,1],col="red")
plot((aa[,2]))
lines(bb[,2],col="red")
plot((aa[,3]))
lines(bb[,3],col="red")
mean(aa[,1])
plot(aa[,1],bb[,1])
plot(aa[,2],bb[,2])
plot(aa[,3],bb[,3])
plot(data1$Sejr)
plot(cumsum(aa[,3]))
mean(aa[,1])
cumsum(aa[,1])
plot(a)
temp <- as.data.frame(data1)
corrplot(as.data.frame(data1$Sejr)~as.data.frame(data1$H))

ggplot(temp,aes(x = as.factor(Sejr),y=Sejr,fill = Sejr)) + geom_boxplot() + xlab("Udfald") +
  geom_hline(yintercept = (sum(dh$Hsejr)+sum(du$Usejr))/33,color = "red",linetype="dashed") +
  geom_hline(yintercept = (sum(dh$Usejr)+sum(du$Hsejr))/33,color = "green", linetype="dashed") +
  geom_hline(yintercept = (sum(dh$Uafgjort)+sum(du$Uafgjort))/33,color = "blue", linetype="dashed") +
  scale_linetype_manual(name = "Observerede procenter", values = c(2, 2),
                        guide = guide_legend(override.aes = list(color = c("red", "blue","green"))))
plot(sort(abs(aa[,1])))
#lines(ppoints(aa[,1]))
lines(ppoints(data1$Hsejr))
ggplot()
ggplot(temp,aes(x = as.factor(Udfald),y=Sandsynlighed,fill = Udfald)) + geom_boxplot() + xlab("Udfald") +
  geom_hline(yintercept = (sum(dh$Hsejr)+sum(du$Usejr))/33,color = "red",linetype="dashed") +
  geom_hline(yintercept = (sum(dh$Usejr)+sum(du$Hsejr))/33,color = "green", linetype="dashed") +
  geom_hline(yintercept = (sum(dh$Uafgjort)+sum(du$Uafgjort))/33,color = "blue", linetype="dashed") +
  scale_linetype_manual(name = "Observerede procenter", values = c(2, 2),
                        guide = guide_legend(override.aes = list(color = c("red", "blue","green"))))

#############ssh vs udfald end
############LRT test
normstyrker = n$styrker/min(n$styrker) #normaliserer styrkerne
styrker = normstyrker;beta = n$beta;theta=n$theta;
Sandsynligheder(beta,theta,x,1,2)
beta = c(rep(0,length(x[,1])))
L1 = -188.9955
L2 = -204.7076
#LRT
chi1 = -2*(L2-L1)
chi2 = pchisq(chi1,11,lower = F);chi2

#Kummulerede sandsynligheder
r[5,12]*ssh(beta,theta,x,5,13)

#Dynamisk model
MeanStyrker = 0;MeanBeta = 0; MeanTheta = 0; MeanSD = 0;
startrunde = 4;PointSSH = 0;PointSSH = c(0,0,0,"FCK","FCK","NN")
Fejl <- c(rep(0,length(unique(data$runde))-13))
for (runde in startrunde:(length(unique(data$runde))-4)) {
#Evaluerer modellen, ift. sandsynligheder for udfald mod de observerede udfald i næste runde
#Finder samtlige parvise sandsynligheder for ALLE sammenligninger
data1 <- data[which((data$runde <= runde) & (data$dato>="2015-07-19") & (data$dato<="2016-05-29")),]
m <- CreateMatrixes(data1,"2015-07-19","2016-05-29",0,FALSE)
x <- m$DesignMatrix;Y <- m$KontingensTabel; r <- m$SamledeKampe;
f <- BTFunktioner(x=x)
n <- NR(x,f);beta = n$beta;theta=n$theta;
styrker = n$styrker;
UnikHold <- sort(unique(data1$H))
counter = 1;KumSSH <- matrix(NA, nrow = (factorial(length(UnikHold))/(factorial((length(UnikHold)-2)))), ncol = 5)
m <- CreateMatrixes(data,"2015-07-19","2016-05-29",0,FALSE)
x <- m$DesignMatrix;Y <- m$KontingensTabel; r <- m$SamledeKampe;
f <- BTFunktioner(x=x)
for (hold1 in 1:length(UnikHold)) {
  for (hold2 in 1:length(UnikHold)) {
      if (hold1 != hold2){
      KumSSH[counter,] <- cbind(Sandsynligheder(theta,x,hold1,hold2)[1],Sandsynligheder(theta,x,hold1,hold2)[2],Sandsynligheder(theta,x,hold1,hold2)[3],UnikHold[hold1],UnikHold[hold2])
      counter = counter +1
      }
    }
}
#Fikser data og sætter Holdene sammen i én column
KumSSH <- as.data.frame.matrix(KumSSH)
names(KumSSH) <- c("H","U","Uafgjort","H1","H2")
KumSSH$H1H2 <- paste(KumSSH$H1,KumSSH$H2)
#KumSSH <- KumSSH[,c(1:3,6)]
KumSSH[,1]<-as.numeric(paste(KumSSH[,1]));KumSSH[,2] <- as.numeric(paste(KumSSH[,2]));KumSSH[,3] <- as.numeric(paste(KumSSH[,3]));
#Finder næste rundes kampe for at evaluere
data1 <- data[which((data$runde==(runde+1)) & (data$dato>="2015-07-19") & (data$dato <= "2016-05-29")),]
data1$HU <- paste(data1$H,data1$U)
#Finder dertilhørende ssh
KumSSH <- KumSSH[KumSSH$H1H2 %in% data1$HU,];
PointSSH <- rbind(PointSSH,KumSSH)
Observationer <- as.data.frame.matrix(cbind(data1$Hsejr,data1$Usejr,data1$Uafgjort,data1$HU));
Observationer <- Observationer[order(Observationer$V4),];Observationer[,1] <- as.numeric(paste(Observationer[,1]));Observationer[,2] <- as.numeric(paste(Observationer[,2]));Observationer[,3] <- as.numeric(paste(Observationer[,3]));
Fejl[runde-9] = sum(Observationer[,1:3]-(KumSSH[,1:3]*Observationer[,1:3]))
MeanStyrker = MeanStyrker + n$styrker/(min(n$styrker))
MeanBeta= MeanBeta + n$beta
MeanTheta = MeanTheta + n$theta
MeanSD = MeanSD + n$sd
#cat("\n",n$sd,"\n")
}
MeanTheta/(runde-4)
MeanBeta/(runde-4)
exp(t(x)%*%beta)/min(exp(t(x)%*%beta))
n$styrker/min(n$styrker)
MeanSD/(runde-3)
(MeanStyrker/(runde-4))/(min(MeanStyrker)/(runde-4))
MeanStyrker/min(MeanStyrker)
styrker = n$styrker/min(n$styrker)

#Til at estimere point, ssh*point for udfald, eks: (0.34,0.26,0.5)*(3,3,1)
data1 <- data[which((data$dato>="2015-07-19") & (data$dato<="2016-05-29")),]
UnikHold <- sort(unique(data1$H))
counter = 1;KumSSH <- matrix(NA, nrow = (factorial(length(UnikHold))/(factorial((length(UnikHold)-2)))), ncol = 5)
m <- CreateMatrixes(data,"2015-07-19","2016-05-29",0,FALSE)
x <- m$DesignMatrix;Y <- m$KontingensTabel; r <- m$SamledeKampe;
f <- BTFunktioner(x=x)
x
n = NR(x,f);
styrker = n$styrker;theta = n$theta; beta = n$beta;
for (hold1 in 1:length(UnikHold)) {
  for (hold2 in 1:length(UnikHold)) {
    if (hold1 != hold2){
      KumSSH[counter,] <- cbind(Sandsynligheder(theta = theta,x = x,i = hold1, j = hold2)[1],Sandsynligheder(theta = theta,x = x,i = hold1, j = hold2)[2],Sandsynligheder(theta = theta,x = x,i = hold1, j = hold2)[3],UnikHold[hold1],UnikHold[hold2])
      counter = counter +1
    }
  }
}
KumSSH <- as.data.frame.matrix(KumSSH)
names(KumSSH) <- c("H","U","Uafgjort","H1","H2")
#KumSSH$H1H2 <- paste(KumSSH$H1,KumSSH$H2)
KumSSH[,1]<-as.numeric(paste(KumSSH[,1]));KumSSH[,2] <- as.numeric(paste(KumSSH[,2]));KumSSH[,3] <- as.numeric(paste(KumSSH[,3]));
sort(KumSSH)
plot(sort(KumSSH$H[which(KumSSH$H1 == "FCK")]))
lines(sort(KumSSH$U[which(KumSSH$H1 == "FCK")]))
lines(KumSSH$Uafgjort[which(KumSSH$H1 == "FCK")])
######## plot start
temp <- cbind(KumSSH$H[which(KumSSH$H1 == "FCK")],KumSSH$U[which(KumSSH$H1 == "FCK")],KumSSH$Uafgjort[which(KumSSH$H1 == "FCK")])
temp <- cbind(temp,cbind(c(rep(1,11)),c(rep(2,11)),c(rep(3,11))))
temp<- as.data.frame(temp)
#temp <- cbind(temp,cbind(c(rep("Sejr",11)),c(rep("Tab",11)),c(rep("Uafgjort",11))))
#temp[,1] <- as.numeric(temp[,1])
temp = cbind(c(temp[,1],temp[,2],temp[,3]),c(temp[,4],temp[,5],temp[,6]))
temp<- as.data.frame(temp)
names(temp) <- c("Sandsynlighed","Udfald")
temp$Udfald <- factor(temp$Udfald, labels = c("Sejr","Tab","Uafgjort"))
ggplot(temp,aes(x = as.factor(Udfald),y=Sandsynlighed,fill = Udfald)) + geom_boxplot() + xlab("Udfald") +
 geom_hline(yintercept = (sum(dh$Hsejr)+sum(du$Usejr))/33,color = "red",linetype="dashed") +
  geom_hline(yintercept = (sum(dh$Usejr)+sum(du$Hsejr))/33,color = "green", linetype="dashed") +
  geom_hline(yintercept = (sum(dh$Uafgjort)+sum(du$Uafgjort))/33,color = "blue", linetype="dashed") +
  scale_linetype_manual(name = "Observerede procenter", values = c(2, 2),
                      guide = guide_legend(override.aes = list(color = c("red", "blue","green"))))
############################ plot end
boxplot(temp[,1:3]~temp[,4:6],main= "FCK",names = c("Sejr","Tab","Uafgjort"))
dh <- data1[which(data1$H=="FCK"),]
du <- data1[which(data1$U=="FCK"),]
abline(h=(sum(dh$Hsejr)+sum(du$Usejr))/33,col = "red",col = "Sejr")
abline(h=(sum(dh$Usejr)+sum(du$Hsejr))/33,col = "blue")
abline(h=(sum(dh$Uafgjort)+sum(du$Uafgjort))/33,col = "green")
text(3.5,(sum(dh$Hsejr)+sum(du$Usejr))/33+0.03,"1",col="black")
text(3.5,(sum(dh$Usejr)+sum(du$Hsejr))/33+0.03,"2",col="black")
text(3.5,(sum(dh$Uafgjort)+sum(du$Uafgjort))/33+0.03,"3",col="black")
?boxplot

t2 <- c(data1$Sejr,data1$Sejr)
plot((t2-1)/2)
lines(1:396,sort(temp))
temp <- c(KumSSH[,1],KumSSH[,2],KumSSH[,3])
loe <- loess((t2-1)/2 ~ c(1:392),control = loess.control(surface = "direct"))
ord <- order(data1$Hsejr+data1$Usejr+data1$Uafgjort)
lines(1:392,sort(loe$fitted[1:392]),col = 2)
sort(loe$fitted[1:392])

KumSSH[,1:3]=KumSSH[,1:3]*1.5
PointSSH[,1:3] <- PointSSH[,1:3]/1.5
KumSSH <- PointSSH
Point = rbind(c(as.character(aggregate(KumSSH$H, by = list(UnikHold = KumSSH$H1),FUN = sum)[,1])),c((aggregate(KumSSH$H, by = list(UnikHold = KumSSH$H1),FUN = sum)[,2]*3+aggregate(KumSSH$U, by = list(UnikHold = KumSSH$H2),FUN = sum)[,2]*3
        +aggregate(KumSSH$Uafgjort, by = list(UnikHold = KumSSH$H1),FUN = sum)[,2]+aggregate(KumSSH$Uafgjort, by = list(UnikHold = KumSSH$H2),FUN = sum)[,2])))

PointSSH[,1] = as.numeric(PointSSH[,1]);PointSSH[,2] = as.numeric(PointSSH[,2]);PointSSH[,3] = as.numeric(PointSSH[,3])
PointDyn <- rbind(c(as.character(aggregate(PointSSH$H,by = list(UnikHold = PointSSH$H1),FUN = sum)[,1])),c((aggregate(PointSSH$H,by = list(UnikHold = PointSSH$H1),FUN = sum)[,2]*3
+aggregate(PointSSH$U,by = list(UnikHold = PointSSH$H2),FUN = sum)[,2]*3
+aggregate(PointSSH$Uafgjort,by = list(UnikHold = PointSSH$H1),FUN = sum)[,2]
+aggregate(PointSSH$Uafgjort,by = list(UnikHold = PointSSH$H2),FUN = sum)[,2])))
FixedDyn <- as.numeric(PointDyn[2,])*(1+3/33)
FixedDyn <- rbind(c(FixedDyn),c(PointDyn[1,]))
sum(as.numeric(FixedDyn[1,]))

