#git add Probabilities.R #git commit -m "Navn på ændring" #git push #git pull
rm(list=ls())
library(matlib) #Til vektor/matrix regning
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
m <- CreateMatrixes(data,"2015-07-19","2016-05-29",0,TRUE)
x <- m$DesignMatrix;Y <- m$KontingensTabel; r <- m$SamledeKampe;
f <- BTFunktioner(x=x)
n <- NR(x,f)
n$beta
n$sd
n$theta

inv(as.matrix(x[,1:11]))

normstyrker = n$styrker/min(n$styrker) #normaliserer styrkerne
styrker = normstyrker;beta = n$beta;theta=n$theta;
Sandsynligheder(beta,theta,x,1,2)
beta = c(rep(0,length(x[,1])))
L1 = -189.452
L2 = -209.046
#LRT
chi1 = -2*(L2-L1)
chi2 = pchisq(chi1,11,lower = F)

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
styrker
for (hold1 in 1:length(UnikHold)) {
  for (hold2 in 1:length(UnikHold)) {
    if (hold1 != hold2){
      KumSSH[counter,] <- cbind(Sandsynligheder(theta = theta,x = x,i = hold1, j = hold2)[1],Sandsynligheder(theta = theta,x = x,i = hold1, j = hold2)[2],Sandsynligheder(theta = theta,x = x,i = hold1, j = hold2)[3],UnikHold[hold1],UnikHold[hold2])
      counter = counter +1
    }
  }
}

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

