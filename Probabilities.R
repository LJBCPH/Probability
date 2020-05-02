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
m <- CreateMatrixes(data,"2015-07-17","2016-05-29",33)
x <- m$DesignMatrix;Y <- m$KontingensTabel; r <- m$SamledeKampe;
n <- NR(x=x)
round(n$beta,6)
round(n$sd,6)
round(n$theta,6)
f <- BTFunktioner(x=x,Y=Y,r=r)
#x <- lapply(x[-1,], function(y) y/sum(y))
as.data.frame(x)
prop.tablet(t(x)/rowSums(x))
prop.table(as.matrix(x),1)
as.data.frame.matrix(prop.table(as.matrix(x),1))
alpha=33
styrker = 0;y = 0;R=0;X=0;FF=0;SamlStyrker=0;
for(alpha in 2:33){
  styrker = 0;y = 0;R=0;X=0;FF=0;
  for (runde in 2:33){
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
  FF <- FF + f$loglike(beta,theta,x)
  R <- R + r
  y <- y + Y
  X <- X + x
}
  #cat(alpha,"   ",FF,"\n",names(x),"\n",styrker/min(styrker),"\n")
}
styrknorm <- styrker/min(styrker)
(styrknorm)[order(styrknorm),]

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
#Sandsyligheder vs udfald
m <- CreateMatrixes(data,"2015-07-17","2016-05-29",4,TRUE)
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
?cast
logitgof(relevel(factor(obs$V1),'3'),Expect,10)
#logitgof(relevel(factor(mtcars$gear),'3'),fitted(mod5))
cc <- as.data.frame(cc)
sump <- aggregate(as.numeric(as.character(cc[,1])),by = list(UnikHold=cc[,4]),FUN = sum)[,2]*3+aggregate(as.numeric(as.character(cc[,2])),by = list(UnikHold=cc[,5]),FUN = sum)[,2]*3+
aggregate(as.numeric(as.character(cc[,3])),by = list(UnikHold=cc[,4]),FUN = sum)[,2]+aggregate(as.numeric(as.character(cc[,3])),by = list(UnikHold=cc[,5]),FUN = sum)[,2]
names(sump) <- names(x)
sump*(1+2/33)

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
dh <- data1[which(data1$H=="FCK"),]
du <- data1[which(data1$U=="FCK"),]
cc <- as.data.frame(cc)
temp <- as.data.frame(cbind(c(as.numeric(as.character(cc$V1)),as.numeric(as.character(cc$V2)),as.numeric(as.character(cc$V3))),c(rep("Hjemmesejr",186),rep("Udesejr",186),rep("Uafgjort",186))))
colnames(temp) <- c("Estimeret Sandsynlighed","Udfald")
temp$`Estimeret Sandsynlighed` <- as.numeric(as.character(temp$`Estimeret Sandsynlighed`))

str(temp)
ggplot(temp,aes(x = as.factor(Udfald),y=`Estimeret Sandsynlighed`,fill = Udfald)) + geom_boxplot() + xlab("Udfald") +
#  geom_hline(yintercept = (mean(data1$Hsejr)),color = 'red',linetype="dashed",size=1) +
#  geom_hline(yintercept = (mean(data1$Usejr)),color = 'blue', linetype="dashed",size=1) +
#  geom_hline(yintercept = (mean(data1$Uafgjort)),color = 'green', linetype="dashed",size=1) +
  geom_hline(aes(yintercept= mean(data1$Hsejr), linetype = "Obs. Hjemesejr"), colour= 'red',size=1) +
  geom_hline(aes(yintercept= mean(data1$Usejr), linetype = "Obs. Udesejr"), colour= 'blue',size=1) +
  geom_hline(aes(yintercept= mean(data1$Uafgjort), linetype = "Obs. Uafgjort"), colour= 'green',size=1) +
  scale_linetype_manual(name = "Observerede procenter", values = c(2, 2,2),
                        guide = guide_legend(override.aes = list(color = c("red", "blue","green"))))

mean(as.numeric(as.character(cc$V3)))

geom_hline(aes(yintercept= 10, linetype = "NRW limit"), colour= 'red') +
  geom_hline(aes(yintercept= 75.5, linetype = "Geochemical atlas limit"), colour= 'blue') +
  scale_linetype_manual(name = "limit", values = c(2, 2),
                        guide = guide_legend(override.aes = list(color = c("blue", "red"))))

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
NR(x=x)
Lmle = -181.0983
Lh0 = -198.0325
#LRT
chi1 = -2*(Lh0-Lmle)
chi2 = pchisq(chi1,12,lower = F);chi2

#Kummulerede sandsynligheder
r[5,12]*ssh(beta,theta,x,5,13)

#Forsøg på standardfejl på styrker
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
#Laver matricen
sdmat <- matrix(nc=length(styrker),nr=length(styrker));SamlStyrker = SamlStyrker/min(SamlStyrker)
for(i in 1:length(SamlStyrker)){
  for(j in 1:length(styrker)){
    if(i == j){
      sdmat[i,i] = dlpipi(SamlStyrker,n$theta,y,R,i)
    }else {
      sdmat[i,j]= dlpipj(SamlStyrker,n$theta,y,R,i,j)
    }
  }
}
sdsty <- sqrt(diag(inv(-sdmat)))
names(sdsty) <- rownames(styrker)
SamlStyrker
sdsty
#Done sd styrker


########OLD Dynamisk model########33
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

