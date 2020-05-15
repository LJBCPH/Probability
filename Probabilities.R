#git add Probabilities.R #git commit -m "Navn p√• √¶ndring" #git push #git pull
rm(list=ls())
library(matlib) #Til vektor/matrix regning
library(MASS) # Til invers af numerisk problematiske matricer
library(blockmatrix) #matrixops√¶tning
library(tidyr) #Til data transformation
library(xtable) #Til Latex table
library(BTSoccer)
library(ggplot2) #plots
library(psych) #pairs.panel
library(SSLASSO)
library(KernSmooth)

setwd("C:/Users/Victo/Desktop/bachelor/kode")
setwd("C:/Users/lucas/Desktop/Odd")
#Henter og verificerer data
data <- read.table("Kampe_r1.csv",header=T,sep=",")
data$H <- as.character(data$H)
data$U <- as.character(data$U)
data$dato <- as.Date(data$dato, format = "%m/%d/%Y")
m <- CreateMatrixes(data,"2015-07-17","2016-05-29",30)
x <- m$DesignMatrix;Y <- m$KontingensTabel; r <- m$SamledeKampe;
KrydsVal <- cbind(c(4,8),c(9,13),c(14,18),c(19,23),c(24,28),c(29,33))
LambdaValues <- c(0,0.5,0.75,1,1.25,1.5,2,2.5,3,3.5,4,4.5,5,6,10,12.5,15,17.5,20,25,35)
for(Lambda in LambdaValues){
  for(Kryds in 1:6){
    SS <- c(KrydsVal[1,Kryds],KrydsVal[2,Kryds])
    RoundList = c(3:(SS[1]-1),(SS[2]+1):33)
    n <- NR(x=x,,Stheta = 1.57,Sbeta = beta0,lambda=Lambda,RoundList = RoundList,MaxIte = 600,eps = 0.0000001,LambLimit = 10^(-4),c = 0.000000000000000001);beta <- n$beta;theta <- n$theta;
  }
}
TData <- data[which((data$runde >= SS[1] & data$runde <= SS[2])&(data$dato>="2015-07-17") & (data$dato <= "2016-05-29")),]
#Sandsynligheder ift. beta osv
a = 0;b=0;aa=0;bb=0;
aa <- as.matrix(rbind(c(0,0,0)));bb <- as.matrix(rbind(c(0,0,0)));cc <- as.matrix(rbind(c(0,0,0,"H","U")));
beta <- n$beta;theta <- n$theta
for (runde in SS[1]:SS[2]){
  m <- CreateMatrixes(data,"2015-07-17","2016-05-29",runde,TRUE)
  x <- m$DesignMatrix;Y <- m$KontingensTabel; r <- m$SamledeKampe;f <- BTFunktioner(x=x,Y=Y,r=r);
  data1 <- data[which((data$dato>="2015-07-17") & (data$dato<="2016-05-29") & (data$runde == runde)),]
  data1$HU <- paste(data1$H,data1$U)
  x <- x[which(rownames(x) %in% names(beta)),]
  styrker <- exp(t(x)%*%beta)
  #styrker <- statstyrker
  counter = 1;KumSSH <- matrix(NA, nrow = (factorial(length(UnikHold))/(factorial((length(UnikHold)-2)))), ncol = 5)
  for (hold1 in 1:length(UnikHold)) {
    for (hold2 in 1:length(UnikHold)) {
      if (hold1 != hold2){
        KumSSH[counter,] <- cbind(Sandsynligheder(theta,x,styrker,hold1,hold2)[1],Sandsynligheder(theta,x,styrker,hold1,hold2)[2],Sandsynligheder(theta,x,styrker,hold1,hold2)[3],UnikHold[hold1],UnikHold[hold2])
        counter = counter +1
      }
    }
  }
  KumSSH <- as.data.frame.matrix(KumSSH) #kummuleret ssh
  names(KumSSH) <- c("H","U","Uafgjort","H1","H2")
  KumSSH$H1H2 <- paste(KumSSH$H1,KumSSH$H2)
  ssh <- KumSSH[KumSSH$H1H2 %in% data1$HU,]
  ssh$H <- as.numeric(as.character(ssh$H));ssh$U <- as.numeric(as.character(ssh$U));ssh$Uafgjort <- as.numeric(as.character(ssh$Uafgjort))
  ssh <- ssh[order(ssh$H1H2),]
  data1 <- data1[order(data1$HU),]
  as.data.frame(lapply((cbind(data1$Hsejr,data1$Usejr,data1$Uafgjort)-cbind(ssh$H,ssh$U,ssh$Uafgjort))^2, function(y) sum(y)))
  A <- c(rowSums(as.matrix((cbind(data1$Hsejr,data1$Usejr,data1$Uafgjort)-cbind(ssh$H,ssh$U,ssh$Uafgjort))^2))) #(Y-Yhat)^2
  a <- c(append(a,A,after= length(a)))
  B <- c(rowSums((cbind(ssh$H,ssh$U,ssh$Uafgjort)^2))) #Yhat^2
  b <- c(append(b,B,after = length(b)))
  AA <- (as.matrix((cbind(data1$Hsejr,data1$Usejr,data1$Uafgjort)-cbind(ssh$H,ssh$U,ssh$Uafgjort)))) #Y-Yhat
  aa <- rbind(aa,AA)
  BB <- as.matrix(((cbind(ssh$H,ssh$U,ssh$Uafgjort)))) #Yhat
  bb <- rbind(bb,BB)
  CC <- as.matrix(((cbind(ssh$H,ssh$U,ssh$Uafgjort,data1$H,data1$U)))) #Yhat med hold
  cc <- rbind(cc,CC)
}
#startbeta = beta

n <- length(beta0)
#fixer length
length(beta305) <- n
#length(Beta0) <- n
#names(beta305) <- c("FifaRating")
beta05
Beta05 <- as.data.frame(beta05,row.names = names(beta05))
BetaList <- list(Beta0,Beta05,Beta075,Beta1,Beta0125,Beta015,Beta2,Beta25,Beta3,Beta35,Beta4,Beta45,Beta5,Beta6,
                 Beta10,Beta125,Beta15,Beta175,Beta20,Beta205,Beta305)
#b1
#b0125 <- sum(abs(beta));beta0125 <- beta #3  -183.8938, 9 beta
#b015 <- sum(abs(beta));beta015 <- beta #3 -184.1924, 8 beta
#b2 <- sum(abs(beta));beta2 <- beta #4, -184.7497, 8 beta
#b25 <- sum(abs(beta));beta25 <- beta, #-185.2661, 7 beta
#b3 <- sum(abs(beta));beta3 <- beta, #-185.7535, 7 beta
#b35 <- sum(abs(beta));beta35 <- beta #10, -186.2123, 7 beta
#b4 <- sum(abs(beta));beta4 <- beta #15, -186.6428, 7 beta
#b45 <- sum(abs(beta));beta45 <- beta #15, -186.0451, 6 beta
#b5 <- sum(abs(beta));beta5 <- beta #20, -187.43, 5 beta
#b6 <- sum(abs(beta));beta6 <- beta #25, -188.1695, 5 beta
#b10 <- sum(abs(beta));beta10 <- beta #30, -190.7334, 5 beta
#b125 <- sum(abs(beta));beta125 <- beta #85,-192.1757, 4 beta
#b15 <- sum(abs(beta));beta15 <- beta #85,-190.7295, 3 beta
#b175 <- sum(abs(beta));beta175 <- beta #85,-192.2547, 3 beta
#b20 <- sum(abs(beta));beta20 <- beta #85,-194.8933, 3 beta
#b205 <- sum(abs(beta));beta205 <- beta #16,-193.4999, 2 beta
#beta305 <- c(0.03629097) #-198.5425, 1 beta

#library(plyr)
#join_all(BetaList,type="full")

for(i in 1:length(BetaList)){
  colnames(BetaList[[i]]) <- paste0( names(BetaList)[i], "_", colnames(BetaList[[i]]) )
  BetaList[[i]]$ROWNAMES  <- rownames(BetaList[[i]])
}
length(BetaList)
m1 <- merge(BetaList[1],BetaList[2],by="ROWNAMES",all=T)

merge(m1,BetaList[3],by="ROWNAMES",all=T)
for(i in 3:21){
  m1 <- merge(m1,BetaList[i],by="ROWNAMES",all=T)
  #m1 <- m1[(length(m1[,1])-11):length(m1[,1]),]
  m1 <- m1[which(m1$ROWNAMES!=""),]
}
m2 <- m1[-18]
m2 <- m2[-10]
m2[is.na(m2)] = 0
m3 <- as.data.frame(t(m2))
colnames(m3) = m2$ROWNAMES
m3 <- m3[-1,]
m3 <- as.data.frame(m3)
LambdaValues <- c(0,0.5,0.75,1,1.25,1.5,2,2.5,3,3.5,4,4.5,5,6,10,12.5,15,17.5,20,25,35)
LambdaValues <- LambdaValues[-18]
LambdaValues <- LambdaValues[-10]
#rownames(m3) <- c(LambdaValues)
m3$Lambda <- c(LambdaValues)
m4 <- read.table(file.choose(),header=T,sep=",")
names(m4) <- c("X","Lambda","variable","value")
names(m4)
m4 <- melt(m3,id.vars="Œª")
m4$value <- as.numeric(m4$value)
m4

ggplot(m4, aes(Lambda,value, col=variable)) +
  geom_line() + ylab("Koefficienter") +xlab("Œª") +  #expand_limits(x = c(LambdaValues))
  geom_hline(aes(yintercept= 0), colour= 'white',size=0.1) +
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35,47))+
  scale_color_manual(values = c("coral4","dodgerblue4","antiquewhite4","deeppink1","chartreuse1","slateblue",
                                "burlywood","magenta1","blue1","orangered1","turquoise3","yellowgreen")) +
#scale_fill_viridis_d() +
  geom_dl(aes(label = variable), method = list(dl.combine("first.points"), cex = 0.8))

  scale_fill_brewer(palette="Spectral",guide="none")
scale_linetype_manual(name = "Observerede %-Udfald", values = c(rep(2,12)),
                      guide = guide_legend(override.aes = list(color = c("red", "blue","green"))))

#write.csv(m4, file = "beta_Lasso.csv")

#b0 <- sum(abs(startbeta));beta0 <- startbeta #0, -181.968, 12 beta
#b05 <- sum(abs(beta));beta05 <- beta #1 -182.8425, 11 beta
#b075 <- sum(abs(beta));beta075 <- beta #2 -183.0041, 10 beta
#b1 <- sum(abs(beta));beta1 <- beta #3  -183.5719, 9 beta
#b125 <- sum(abs(beta));beta125 <- beta #3  -183.8938, 9 beta
#b15 <- sum(abs(beta));beta15 <- beta #3 -184.1924, 8 beta
#b2 <- sum(abs(beta));beta2 <- beta #4, -184.7497, 8 beta
#b25 <- sum(abs(beta));beta25 <- beta, #-185.2661, 7 beta
#b3 <- sum(abs(beta));beta3 <- beta, #-185.7535, 7 beta
#b35 <- sum(abs(beta));beta35 <- beta #10, -186.2123, 7 beta
#b4 <- sum(abs(beta));beta4 <- beta #15, -186.6428, 7 beta
#b45 <- sum(abs(beta));beta45 <- beta #15, -186.0451, 6 beta
#b5 <- sum(abs(beta));beta5 <- beta #20, -187.43, 5 beta
#b6 <- sum(abs(beta));beta6 <- beta #25, -188.1695, 5 beta
#b10 <- sum(abs(beta));beta10 <- beta #30, -190.7334, 5 beta
#b125 <- sum(abs(beta));beta125 <- beta #85,-192.1757, 4 beta
#b15 <- sum(abs(beta));beta15 <- beta #85,-190.7295, 3 beta
#b175 <- sum(abs(beta));beta175 <- beta #85,-192.2547, 3 beta
#b20 <- sum(abs(beta));beta20 <- beta #85,-194.8933, 3 beta
#b205 <- sum(abs(beta));beta205 <- beta #16,-193.4999, 2 beta
#b35 <- c(0.03629097) #-198.5425, 1 beta

alpha=33
styrker = 0;y = 0;R=0;X=0;FF=0;SamlStyrker=0;
for(alpha in 2:33){
  styrker = 0;y = 0;R=0;X=0;FF=0;SumLike = 0;
  for (runde in 3:33){
  if(runde<=alpha){
    alphaback = 1
  } else {
    alphaback = runde-alpha
  }
  data1 <- data[which((data$dato>="2015-07-17") & (data$dato<="2016-05-29") & (data$runde>=alphaback)),]
  m <- CreateMatrixes(data1,"2015-07-17","2016-05-29",runde)
  x <- m$DesignMatrix;Y <- m$KontingensTabel; r <- m$SamledeKampe;f <- BTFunktioner(x=x,Y=Y,r=r);
  styrker <- styrker+(exp(t(x)%*%beta));
  #SamlStyrker <- SamlStyrker + styrker
  toprint <- exp(t(x)%*%beta)/(min(exp(t(x)%*%beta)))
  FF <- FF + f$loglike(beta,theta,0,x)
  R <- R + r
  y <- y + Y
  X <- X + x
  }

  #cat(alpha,"   ",FF,"\n",names(x),"\n",styrker/min(styrker),"\n")
}
styrknorm <- styrker/min(styrker)
(styrknorm)[order(styrknorm),]
(styrker/31)

beta <- c(rep(0,length(x[1,])))
n <- NR(x=x)
n$styrker/min(n$styrker)
beta <- n$beta;
theta <- n$theta
n$sd
data1 <- data[which((data$dato>="2015-07-17") & (data$dato<="2016-05-29")),]
UnikHold <- unique(data1$H)
UnikHold <- sort(UnikHold)
runde = 3
#Danner sandsynligheder tilh√∏rende kampene
m <- CreateMatrixes(data,"2015-07-17","2016-05-29",4,TRUE) #laver design, kontingens og r-tabel
a = 0;b=0;aa=0;bb=0;beta <- n$beta;theta<-n$theta
aa <- as.matrix(rbind(c(0,0,0)));bb <- as.matrix(rbind(c(0,0,0)));cc <- as.matrix(rbind(c(0,0,0,"H","U")));
for (runde in 1:33){
  #m <- CreateMatrixes(data,"2015-07-17","2016-05-29",runde,TRUE)
  #x <- m$DesignMatrix;Y <- m$KontingensTabel; r <- m$SamledeKampe;f <- BTFunktioner(x=x,Y=Y,r=r);
  data1 <- data[which((data$dato>="2015-07-17") & (data$dato<="2016-05-29") & (data$runde == runde)),]
  data1$HU <- paste(data1$H,data1$U)
  #styrker <- exp(t(x)%*%beta)
  piH <- exp(t(x)%*%beta)
  #styrker <- statstyrker
  counter = 1;KumSSH <- matrix(NA, nrow = (factorial(length(UnikHold))/(factorial((length(UnikHold)-2)))), ncol = 5)
  #for (hold1 in 1:length(UnikHold)) {
  #  for (hold2 in 1:length(UnikHold)) {
  #    if (hold1 != hold2){
  #      KumSSH[counter,] <- cbind(Sandsynligheder(theta,x,styrker,hold1,hold2)[1],Sandsynligheder(theta,x,styrker,hold1,hold2)[2],Sandsynligheder(theta,x,styrker,hold1,hold2)[3],UnikHold[hold1],UnikHold[hold2])
  #      counter = counter +1
  #    }
  #  }
  #}
  #UnikHold=names(piH);counter = 1;KumSSH <- matrix(NA, nrow = (factorial(length(UnikHold))/(factorial((length(UnikHold)-2)))), ncol = 5)
  for (hold1 in 1:length(UnikHold)) {
    for (hold2 in 1:length(UnikHold)) {
      if (hold1 != hold2){
        KumSSH[counter,] <- cbind(Sandsynligheder1(piH,hold1,hold2)[1],Sandsynligheder1(piH,hold1,hold2)[2],Sandsynligheder1(piH,hold1,hold2)[3],UnikHold[hold1],UnikHold[hold2])
       counter = counter +1
      }
   }
  }
  KumSSH <- as.data.frame.matrix(KumSSH) #kummuleret ssh
  names(KumSSH) <- c("H","U","Uafgjort","H1","H2")
  KumSSH$H1H2 <- paste(KumSSH$H1,KumSSH$H2)
  ssh <- KumSSH[KumSSH$H1H2 %in% data1$HU,]
  ssh$H <- as.numeric(as.character(ssh$H));ssh$U <- as.numeric(as.character(ssh$U));ssh$Uafgjort <- as.numeric(as.character(ssh$Uafgjort))
  ssh <- ssh[order(ssh$H1H2),]
  data1 <- data1[order(data1$HU),]
  as.data.frame(lapply((cbind(data1$Hsejr,data1$Usejr,data1$Uafgjort)-cbind(ssh$H,ssh$U,ssh$Uafgjort))^2, function(y) sum(y)))
  A <- c(rowSums(as.matrix((cbind(data1$Hsejr,data1$Usejr,data1$Uafgjort)-cbind(ssh$H,ssh$U,ssh$Uafgjort))^2))) #(Y-Yhat)^2
  a <- c(append(a,A,after= length(a)))
  B <- c(rowSums((cbind(ssh$H,ssh$U,ssh$Uafgjort)^2))) #Yhat^2
  b <- c(append(b,B,after = length(b)))
  AA <- (as.matrix((cbind(data1$Hsejr,data1$Usejr,data1$Uafgjort)-cbind(ssh$H,ssh$U,ssh$Uafgjort)))) #Y-Yhat
  aa <- rbind(aa,AA)
  BB <- as.matrix(((cbind(ssh$H,ssh$U,ssh$Uafgjort)))) #Yhat
  bb <- rbind(bb,BB)
  CC <- as.matrix(((cbind(ssh$H,ssh$U,ssh$Uafgjort,data1$H,data1$U)))) #Yhat med hold
  cc <- rbind(cc,CC)
}
beta
#Fixer data til Hosmer og Lemeshows GOF-test
data1 <- data[which((data$dato>="2015-07-17") & (data$dato<="2016-05-29")),]
ccS <- cc[2:length(cc[,1]),]
cc <- as.data.frame(cc)
ccc <- as.data.frame(cc[,1:3])
Expect <- cbind(as.numeric(as.character(ccc[,1])),as.numeric(as.character(ccc[,3])),as.numeric(as.character(ccc[,2])))
colnames(Expect) <- c("1","2","3")
Expect <- as.data.frame(Expect)
data1 <- data[which((data$dato>="2015-07-17") & (data$dato<="2016-05-29")),]
data1 <- data1[order(data1$runde,data1$H),]
data1 <- data1[13:length(data1[,2]),]
obs <- cbind(data1$Sejr,data1$H,data1$U)
obs <- as.data.frame(obs)
logitgof(relevel(factor(obs$V1),'3'),Expect,10)

#Estimererede Pointtildeling
cc <- as.data.frame(cc);cc <- cc[-1,]
sump <- aggregate(as.numeric(as.character(cc[,1])),by = list(UnikHold=cc[,4]),FUN = sum)[,2]*3+aggregate(as.numeric(as.character(cc[,2])),by = list(UnikHold=cc[,5]),FUN = sum)[,2]*3+
aggregate(as.numeric(as.character(cc[,3])),by = list(UnikHold=cc[,4]),FUN = sum)[,2]+aggregate(as.numeric(as.character(cc[,3])),by = list(UnikHold=cc[,5]),FUN = sum)[,2]
names(sump) <- names(Y)
round(sump*(1+2/33),3)
sum(sump*(1+2/33))
a <- a[2:length(a)]
b <- b[2:length(b)]
aa <- aa[2:length(aa[,1]),1:3]
bb <- bb[2:length(bb[,1]),1:3]
mean(data1$Uafgjort)
aa <- as.data.frame(aa)
bb <- as.data.frame(bb)

#Til residualplots
#ccStatisk <- cc; #SSH for statisk model
aa <- as.data.frame(aa);aa <- aa[-1,]
resDyn <- aa
ccDyn <- cc
colnames(ccStatisk) <- c("Hjemmesejr","Udesejr","Uafgjort","HjemmeHold","UdeHold")
ccStatisk[,1:3] <- sapply(ccStatisk[,1:3],as.character)
ccStatisk[,1:3] <- sapply(ccStatisk[,1:3],as.numeric)
#ccDyn <- cc; #SSH for dynamisk model
colnames(ccDyn) <- c("Hjemmesejr","Udesejr","Uafgjort","HjemmeHold","UdeHold")
ccDyn[,1:3] <- sapply(ccDyn[,1:3],as.character)
ccDyn[,1:3] <- sapply(ccDyn[,1:3],as.numeric)
data1 <- data[which((data$dato>="2015-07-17") & (data$dato<="2016-05-29")),]
data1 <- data1[order(data1$runde,data1$H),]
data1 <- data1[13:length(data1[,2]),]
obs <- cbind(data1$Hsejr,data1$Usejr,data1$Uafgjort,data1$H,data1$U)
obs <- as.data.frame(obs)
colnames(obs) <- c("Hjemmesejr","Udesejr","Uafgjort","HjemmeHold","UdeHold")
obs[,1:3] <- sapply(obs[,1:3],as.character)
obs[,1:3] <- sapply(obs[,1:3],as.numeric)
resDyn <- obs[,1:3]-ccDyn[,1:3];resDyn <- as.data.frame(resDyn)
resStatisk <- obs[,1:3]-ccStatisk[,1:3];resStatisk <- as.data.frame(resStatisk)
ccDyn2 <- ccDyn[,1:3]^2
ccStatisk2 <- ccStatisk[,1:3]^2
resDyn2 <- (obs[,1:3]-ccDyn[,1:3])^2;resDyn <- as.data.frame(resDyn)
resStatisk2 <- (obs[,1:3]-ccStatisk[,1:3])^2;resStatisk <- as.data.frame(resStatisk)
ccDyn
#plots of res vs fitted
#estimerede sandsynligheder
mean(as.numeric(ccDyn[,2]))
sum(as.numeric(ccDyn[,1]))/198#Dynamisk HS
sum(as.numeric(ccDyn[,2]))/198#Dynamsik US
sum(as.numeric(ccDyn[,3]))/198#Dynamisk U
sum(data1$Hsejr)/198#Sandt HS
sum(data1$Usejr)/198#Sandt US
sum(data1$Uafgjort)/198#sandt Uaf
sum(as.numeric(ccDyn[,1]))/186+sum(as.numeric(ccDyn[,2]))/186+sum(as.numeric(ccDyn[,3]))/186
#DYN
plot(ccDyn[,1],resDyn[,1],xlab="Fittede vÊrdier",ylab="RÂ Residualer",main="Hjemmesejr")
lines(locpoly(as.numeric(ccDyn[,1]),resDyn[,1],kernel="normal",bandwidth = 0.06,degree = 0),col="red")
abline(h=mean(resDyn[,1]),col="black")
plot(ccDyn[,2],resDyn[,2],xlab="Fittede vÊrdier",ylab="RÂ Residualer",main="Udesejr")
lines(locpoly(as.numeric(ccDyn[,2]),resDyn[,2],kernel="normal",bandwidth = 0.06,degree=0),col="red")
abline(h=mean(resDyn[,2]),col="black")
plot(ccDyn[,3],resDyn[,3],xlab="Fittede vÊrdier",ylab="RÂ Residualer",main="Uafgjort")
lines(locpoly(as.numeric(ccDyn[,3]),resDyn[,3],kernel="normal",bandwidth = 0.02,degree=0),col="red")
abline(h=mean(resDyn[,3]),col="black")
#STATISK
plot(ccStatisk[,1],resStatisk[,1],xlab="Fitted",ylab="Raw residuals",main="Hjemmesejr")
lines(locpoly(ccStatisk[,1],resStatisk[,1],kernel="normal",bandwidth = 0.05),col="red")
abline(h=mean(resStatisk[,1]),col="black")
plot(ccStatisk[,2],resStatisk[,2],xlab="Fitted",ylab="Raw residuals",main="Udesejr")
lines(locpoly(ccStatisk[,2],resStatisk[,2],kernel="normal",bandwidth = 0.05),col="red")
abline(h=mean(resStatisk[,2]),col="black")
plot(ccStatisk[,3],resStatisk[,3],xlab="Fitted",ylab="Raw residuals",main="Uafgjort")
lines(locpoly(ccStatisk[,3],resStatisk[,3],kernel="normal",bandwidth = 0.02),col="red")
abline(h=mean(resStatisk[,3]),col="black")
resDyn[order(resDyn[,1]),]
dpill(as.numeric(ccDyn[,1]),resDyn[,1])
dpill(as.numeric(ccDyn[,2]),resDyn[,2])
dpill(as.numeric(ccDyn[,3]),resDyn[,3])
?dpill
str(ccDyn)
str(resDyn)
#GGplot - box
str(temp)
ggplot(temp,aes(x = as.factor(Udfald),y=`Estimeret Sandsynlighed`,fill = Udfald)) + geom_boxplot() + xlab("Udfald") +
  geom_hline(aes(yintercept= mean(data1$Hsejr), linetype = "Obs. %-Hjemesejr"), colour= 'red',size=1) +
  geom_hline(aes(yintercept= mean(data1$Usejr), linetype = "Obs. %-Udesejr"), colour= 'blue',size=1) +
  geom_hline(aes(yintercept= mean(data1$Uafgjort), linetype = "Obs. %-Uafgjort"), colour= 'green',size=1) +
  scale_linetype_manual(name = "Observerede %-Udfald", values = c(2, 2,2),
                        guide = guide_legend(override.aes = list(color = c("red", "blue","green"))))


############LRT test
normstyrker = n$styrker/min(n$styrker) #normaliserer styrkerne
styrker = normstyrker;beta = n$beta;theta=n$theta;
Sandsynligheder(beta,theta,x,1,2)
beta = c(rep(0,length(x[,1])))
NR(x=x)
Lmle = -190.6743
Lh0 = -210.9386
#LRT
chi1 = -2*(Lh0-Lmle)
chi2 = pchisq(chi1,12,lower = F);round(chi2,5)

#Standardfejl p√• styrker
dlpipi <- function(styrker,theta,Y,r,i){
  sum=0;
  for(j in 1:length(styrker)) {
    if(j != i){
        sum = sum + (-((r[i,j]-Y[j,i])/(styrker[i]^2))+
                    ((r[i,j]-Y[j,i])/(styrker[i]+theta*styrker[j])^2)+
                    ((theta*(r[i,j]-Y[i,j]))/(styrker[j]+theta*styrker[i])^2))
      }
  }
  return(sum)
}
dlpipj <- function(styrker,theta,Y,r,i,j){
  sum=((theta*(r[i,j]-Y[j,i]))/(styrker[i]+theta*styrker[j])^2)+
      ((theta*(r[i,j]-Y[i,j]))/(styrker[j]+theta*styrker[i])^2)
  return(sum)
}
dlpi <- function(styrker,theta,Y,r,i){
  sum=0;
  for(j in 1:length(styrker)) {
    if(j != i){
      sum = sum + (r[i,j]-Y[i,j])/(styrker[i])-
        (r[i,j]-Y[j,i])/(styrker[i]+theta*styrker[j])-
        (theta*(r[i,j]-Y[i,j]))/(styrker[j]+theta*styrker[i])
    }
  }
  return(sum)
}
dltheta <- function(styrker,theta,Y,r,i){
  for(j in 1:length(styrker)){
    if(j != i){
      sum = sum + (2*theta(r[i,j]-Y[i,j]-Y[j,i]))/(theta^2-1)-
              (styrker[i]*(r[i,j]-Y[j,i]))/(styrker[j]+theta*styrker[i])-
              (styrker[j]*(r[i,j]-Y[i,j]))/(styrker[i]+theta*styrker[j])
    }
  }
  return(sum)
}
#Laver matricen
sdmat <- matrix(nc=length(styrker),nr=length(styrker));SamlStyrker = SamlStyrker/min(SamlStyrker)
grad <- c(rep(0,length(styrker)))
for(i in 1:length(SamlStyrker)){
  for(j in 1:length(styrker)){
    if(i == j){
      sdmat[i,i] = dlpipi(SamlStyrker,n$theta,y,R,i)
    }else {
      sdmat[i,j]= dlpipj(SamlStyrker,n$theta,y,R,i,j)
    }
    }
      grad[i] = dlpi(styrker,theta,Y,r,i)
}
#sdsty <- sqrt(diag(inv(-sdmat))) wrong?
sdsty <- sqrt(diag(inv(-sdmat))) #richtig?
names(sdsty) <- rownames(styrker)
t(grad)%*%inv(-sdmat)%*%grad
grad
SamlStyrker <- styrker
sdsty
#Done sd styrker
deltaMethod(styrker,rownames(styrker),vcov.=inv(-sdmat))
vcov(sdmat)
##################
n <- NR(x=x,lambda=0,RoundList = c(3:33),MaxIte = 600,eps = 0.0000001,LambLimit = 10^(-4),c = 10^(-100))

beta <- n$beta;theta <- n$theta;names(beta)=rownames(x)
sfb<-n$sd;names(sfb)=rownames(x)
m <- CreateMatrixes(data,"2015-07-17","2016-05-29",30)
x <- m$DesignMatrix;Y <- m$KontingensTabel; r <- m$SamledeKampe
Xgns <- n$Xgns/31;colnames(Xgns)=colnames(Y)
XgnsUHB <- Xgns;betaUHB = beta#fjerne hjemmebane
XgnsH <- XgnsUHB-XgnsUHB[,7]
pignsH <- exp(t(XgnsH)%*%betaUHB); rownames(pigns)=colnames(Y)#GnsStyrker
###################Standardfejl:
varpignsH <- c(rep(0,12));varbeta <- n$KV[-13,-13];varbetaH <-varbeta
for (i in 1:12){
  varpignsH[i]= exp(t(XgnsH[,i])%*%betaUHB)%*%(t(XgnsH[,i])%*%varbetaH%*%XgnsH[,i])%*%exp(t(XgnsH[,i])%*%betaUHB)
}
sfpignsH <- sqrt(varpignsH);
names(varpignsH)=colnames(Y);names(sfpignsH)=colnames(Y);rownames(pignsH)=colnames(Y)
round(varpignsH,3)#varians
round(t(pignsH),3)#styrker
round(sfpignsH,3)#standardfejl

