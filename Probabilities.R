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
setwd("C:/Users/Victo/Desktop/bachelor/kode")
setwd("C:/Users/lucas/Desktop/Odd")
#Henter og verificerer data
data <- read.table("Kampe_r1.csv",header=T,sep=",")
data$H <- as.character(data$H)
data$U <- as.character(data$U)
data$dato <- as.Date(data$dato, format = "%m/%d/%Y")
m <- CreateMatrixes(data,"2015-07-17","2016-05-29",4,TRUE)
x <- m$DesignMatrix;Y <- m$KontingensTabel; r <- m$SamledeKampe;
f <- BTFunktioner(x=x,Y=Y,r=r)

for (runde in 1:33){
m <- CreateMatrixes(data,"2015-07-17","2016-05-29",runde,TRUE)
x <- m$DesignMatrix;Y <- m$KontingensTabel; r <- m$SamledeKampe;
f <- BTFunktioner(x=x,Y=Y,r=r)
  if (runde == 1) {
#    funk <- list(f$loglike(beta,theta,x),f$dlbeta(beta,theta,x),f$dltheta(beta,theta,x),f$dl2xtheta(beta,theta,x),f$dlbetatheta(beta,theta,x))
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

f$t1 <- t1;f$t2 <- t2;f$t3 <- t3;f$t4 <- t4;f$t5 <- t5;f$t6 <- t6;
n <- NR(x,MaxIte = 1000)

beta <- c(0.0954729,0.05781863,3.256189,-0.2238851,0.1585594,-0.4942447,-0.704412,0.08631357,0.06842135,0.08486926,0.3493483,0.03300651)
a12 = as.matrix(f$dlbeta(beta,theta,x));
grad = rbind(a12,f$dltheta(beta,theta,x));
A = f$dl2xbeta(beta,theta,x);B = as.matrix(f$dlbetatheta(beta,theta,x));C = t(as.matrix(f$dlbetatheta(beta,theta,x)));D = f$dl2xtheta(beta,theta,x);
hess = cbind(A,B);hess = rbind(hess,c(C,D));inf=-hess;
n$sd

n <- NR(x,f)
theta=1.01
data[which((data$dato>="2015-07-19")&(data$dato<="2016-05-29")&(data$runde == 1)),]

normstyrker = n$styrker/min(n$styrker) #normaliserer styrkerne
styrker = normstyrker;beta = n$beta;theta=n$theta;
Sandsynligheder(beta,theta,x,1,2)
beta = c(rep(0,length(x[,1])))
L1 = -189.452
L2 = -209.046
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
      KumSSH[counter,] <- cbind(Sandsynligheder(,theta,x,hold1,hold2)[1],Sandsynligheder(,theta,x,hold1,hold2)[2],Sandsynligheder(,theta,x,hold1,hold2)[3],UnikHold[hold1],UnikHold[hold2])
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

