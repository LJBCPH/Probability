#git add Probabilities.R #git commit -m "Navn på ændring" #git push #git pull
rm(list=ls())
library(matlib) #Til vektor/matrix regning
library(MASS) # Til invers af numerisk problematiske matricer
library(blockmatrix) #matrixopsætning
library(tidyr) #Til data transformation
library(xtable) #Til Latex table
library(BTSoccer)
library(ggplot2)
library(psych) #pairs.panel
library(glmnet)
library(SSLASSO)

setwd("C:/Users/Victo/Desktop/bachelor/kode")
setwd("C:/Users/lucas/Desktop/Odd")
#Henter og verificerer data
data <- read.table("Kampe_r1.csv",header=T,sep=",")
data$H <- as.character(data$H)
data$U <- as.character(data$U)
data$dato <- as.Date(data$dato, format = "%m/%d/%Y")
m <- CreateMatrixes(data,"2015-07-17","2016-05-29",30)
x <- m$DesignMatrix;Y <- m$KontingensTabel; r <- m$SamledeKampe;
n <- NR(x=x,,Stheta = 1.57,Sbeta = startbeta[-10],lambda=25,MaxIte = 600,eps = 0.0000001,LambLimit = 10^(-4),c = 0.000000000000000001);beta <- n$beta;theta <- n$theta;
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
##b17 <- sum(abs(beta));beta17 <- beta #125, 0.0000000000001,0,-193.9977, 3 beta
#b18 <- sum(abs(beta));beta18 <- beta #150, 0.0000000000001, 0, -194.3907, 3 beta
##b19 <- sum(abs(beta));beta19 <- beta #50, 0.00000001, 0, -194.6659, 3 beta
#b20 <- sum(abs(beta));beta20 <- beta #50, 0.000000001, 0, -194.9399, 3 beta
#b22 <- sum(abs(beta));beta22 <- beta #50, 0.0000000000001, 0 ,-195.4818 , 3 beta
#b25 <- sum(abs(beta));beta25 <- beta #50, 0.0000000000001, 0 ,-195.4818 , 2 beta
#b25 <- sum(abs(beta));beta25 <- beta #50, 0.0000000000001, 0 ,-197.0296 , 2 beta
#beta35 <- c(0.01903043)

colSums(Bsamlet)
#startbeta = beta
LambdaValues <- c(0,0.5,1,1.5,2,3,3.5,4,5,6,7,10,13,13.5,16,18,20,22,25)
n <- length(beta0)
#fixer length
#length(beta075) <- n
#length(Beta0) <- n
#names(beta4) <- c("FifaRating")
beta1
Beta075 <- as.data.frame(beta075,row.names = names(beta075))
BetaList <- list(Beta0,Beta075,Beta1,Beta15,Beta2,Beta3,Beta4,Beta5,Beta6,Beta7,Beta10,Beta13,Beta135,Beta16,Beta18,Beta20,Beta22,Beta25,Beta35)
#multimerge(BetaList)
#unique(unlist(lapply(BetaList, rownames)))
#library(plyr)
#join_all(BetaList,type="full")
for(i in 1:length(BetaList)){
  colnames(BetaList[[i]]) <- paste0( names(BetaList)[i], "_", colnames(BetaList[[i]]) )
  BetaList[[i]]$ROWNAMES  <- rownames(BetaList[[i]])
}

m1 <- merge(BetaList[1],BetaList[2],by="ROWNAMES",all=T)
for(i in 3:20){
m1 <- merge(m1,BetaList[i],by="ROWNAMES",all=T)
m1 <- m1[(length(m1[,1])-11):length(m1[,1]),]
}
m2 <- m1[-16]
m2 <- m2[-4]
m2[is.na(m2)] = 0
m3 <- as.data.frame(t(m2))
colnames(m3) = m2$ROWNAMES
m3 <- m3[-1,]
m3 <- as.data.frame(m3)
m3$Lambda <- as.numeric(row.names(m3))
m4 <- melt(m3,id.vars="Lambda")
m4$value <- as.numeric(m4$value)
ggplot(m4, aes(Lambda,value, col=variable)) +
  geom_line() + geom_hline()

#b0 <- sum(abs(beta));beta0 <- beta #0, 0.0000000000001, -181.968, 12 beta
#b075 <- sum(abs(beta));beta075 <- beta #1 0.000000000001, 0, -182.1556, 11 beta
#b1 <- sum(abs(beta));beta1 <- beta #2 0.000000000001, 0, -183.5661, 10 beta
#b15 <- sum(abs(beta));beta15 <- beta #3 0.000000000001, 0, -184.189, 10 beta
#b2 <- sum(abs(beta));beta2 <- beta #3 0.000000000001, 0, -184.7619, 9 beta
##b25 <- sum(abs(beta));beta25 <- beta #4, 0.000000000001, 0, -185.2501, 8 beta
#b3 <- sum(abs(beta));beta3 <- beta #5, 0.000000000001, 0, -185.7342, 8 beta
##b35 <- sum(abs(beta));beta35 <- beta #10, 0.000000000001, 0, -186.1894, 8 beta
#b4 <- sum(abs(beta));beta4 <- beta #15, 0.000000000001, 0, -186.6159, 7 beta
#b5 <- sum(abs(beta));beta5 <- beta #20,0.0000000000001, 0, -187.4058, 6 beta
#b6 <- sum(abs(beta));beta6 <- beta #25,0.0000000000001, 0, -188.1516, 6 beta
#b7 <- sum(abs(beta));beta7 <- beta #30,0.0000000000001, 0, , 6 beta
#b10 <- sum(abs(beta));beta10 <- beta #85, 0.000000000001,0,-190.7295, 6 beta
#b13 <- sum(abs(beta));beta13 <- beta #85, 0.000000000001,0,-192.2547, 5 beta
#b135 <- sum(abs(beta));beta135 <- beta #85, 0.000000000001,0,-192.4757, 4 beta
#b16 <- sum(abs(beta));beta16 <- beta #16, 0.0000000000001, 0,-193.4999, 4 beta
##b17 <- sum(abs(beta));beta17 <- beta #125, 0.0000000000001,0,-193.9977, 3 beta
#b18 <- sum(abs(beta));beta18 <- beta #150, 0.0000000000001, 0, -194.3907, 3 beta
##b19 <- sum(abs(beta));beta19 <- beta #50, 0.00000001, 0, -194.6659, 3 beta
#b20 <- sum(abs(beta));beta20 <- beta #50, 0.000000001, 0, -194.9399, 3 beta
#b22 <- sum(abs(beta));beta22 <- beta #50, 0.0000000000001, 0 ,-195.4818 , 3 beta
#b25 <- sum(abs(beta));beta25 <- beta #50, 0.0000000000001, 0 ,-195.4818 , 2 beta
#b25 <- sum(abs(beta));beta25 <- beta #50, 0.0000000000001, 0 ,-197.0296 , 2 beta
#beta35 <- c(0.01903043)

round(beta1,4)#0
round(beta2,4)#0.00000001
round(beta4,4)#0.03
round(beta5,4)#0.04
round(beta3,4)#0.05
sum(abs(beta1))#25,c=0,1
sum(abs(beta2))#100,c=0,01
sum(abs(beta3))#0.3
sum(abs(beta4))#2
sum(abs(beta5))#5
sum(abs(beta6))#10
sum(abs(beta7))#50
sum(abs(beta8))#100
sum(abs(beta9))#200
sum(abs(beta10))#200

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
#Danner sandsynligheder tilhørende kampene
m <- CreateMatrixes(data,"2015-07-17","2016-05-29",4,TRUE) #laver design, kontingens og r-tabel
a = 0;b=0;aa=0;bb=0;
aa <- as.matrix(rbind(c(0,0,0)));bb <- as.matrix(rbind(c(0,0,0)));cc <- as.matrix(rbind(c(0,0,0,"H","U")));
for (runde in 3:33){
  m <- CreateMatrixes(data,"2015-07-17","2016-05-29",runde,TRUE)
  x <- m$DesignMatrix;Y <- m$KontingensTabel; r <- m$SamledeKampe;f <- BTFunktioner(x=x,Y=Y,r=r);
  data1 <- data[which((data$dato>="2015-07-17") & (data$dato<="2016-05-29") & (data$runde == runde)),]
  data1$HU <- paste(data1$H,data1$U)
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

#Fixer data til Hosmer og Lemeshows GOF-test
data1 <- data[which((data$dato>="2015-07-17") & (data$dato<="2016-05-29")),]
cc <- cc[2:length(cc[,1]),]
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
cc <- as.data.frame(cc)
sump <- aggregate(as.numeric(as.character(cc[,1])),by = list(UnikHold=cc[,4]),FUN = sum)[,2]*3+aggregate(as.numeric(as.character(cc[,2])),by = list(UnikHold=cc[,5]),FUN = sum)[,2]*3+
aggregate(as.numeric(as.character(cc[,3])),by = list(UnikHold=cc[,4]),FUN = sum)[,2]+aggregate(as.numeric(as.character(cc[,3])),by = list(UnikHold=cc[,5]),FUN = sum)[,2]
names(sump) <- names(x)

a <- a[2:length(a)]
b <- b[2:length(b)]
aa <- aa[2:length(aa[,1]),1:3]
bb <- bb[2:length(bb[,1]),1:3]
mean(data1$Uafgjort)
aa <- as.data.frame(aa)
bb <- as.data.frame(bb)

#Til residualplots
#ccStatisk <- cc; #SSH for statisk model
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

#plots of res vs fitted
plot(ccDyn[,1],resDyn[,1],xlab="Fitted",ylab="Raw residuals",main="Hjemmesejr")
lines(locpoly(ccDyn[,1],resDyn[,1],kernel="normal",bandwidth = 0.05),col="red")
abline(h=mean(resDyn[,1]),col="black")
plot(ccDyn[,2],resDyn[,2],xlab="Fitted",ylab="Raw residuals",main="Udesejr")
lines(locpoly(ccDyn[,2],resDyn[,2],kernel="normal",bandwidth = 0.05),col="red")
abline(h=mean(resDyn[,2]),col="black")
plot(ccDyn[,3],resDyn[,3],xlab="Fitted",ylab="Raw residuals",main="Uafgjort")
lines(locpoly(ccDyn[,3],resDyn[,3],kernel="normal",bandwidth = 0.02),col="red")
abline(h=mean(resDyn[,3]),col="black")
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
dpill(ccDyn[,1],resDyn[,1])

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

#Standardfejl på styrker
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
