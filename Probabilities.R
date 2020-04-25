#git add Probabilities.R #git commit -m "Navn på ændring" #git push #git pull
rm(list=ls())
library(matlib) #Til vektor/matrix regning
library(blockmatrix) #matrixopsætning
library(tidyr) #Til data transformation
install.packages("C:/Users/Victo/Documents/GitHub/Probability/BTSoccer", 
                 repos = NULL, 
                 type = "source")
library(BTSoccer)
setwd("C:/Users/Victo/Desktop/bachelor/kode")
setwd("C:/Users/lucas/Desktop/Odd")
#Henter og verificerer data
data <- read.table("kampe_r1.csv",header=T,sep=",")
m <- CreateMatrixes(data,"2018-07-15","2019-05-26",0)
x <- m$DesignMatrix;Y <- m$KontingensTabel; r <- m$SamledeKampe;
f <- BTFunktioner(beta,theta,x)
n <- NR(x,f)

head(data)
str(data)
#fikser datatypes
data$H <- as.character(data$H)
data$U <- as.character(data$U)
data$dato <- as.Date(data$dato, format = "%m/%d/%Y")
#Henter 1.5 sæson ud:
data1 <- data[which((data$dato>="2018-07-13") & (data$dato <= "2019-05-26")),]
#Finder holdene der er i Superligaen i indeværende sæson
dataUNan <- na.omit(data1)
detach(data1)
#sætter Y-kontingenstabellen op:
#Samler strengen af hjemme mod ude
data1$HU <- paste(data1$H,data1$U)
UnikSammenligner <- unique(data1$HU)
#Udregner antal hjemmesejre og udesejre i kombinationerne
hjemmesejre <- aggregate(data1$Hsejr, by = list(HU=data1$HU),FUN=sum);hjemmesejre <- hjemmesejre[order(hjemmesejre$HU),]
udesejre <- aggregate(data1$Usejr, by = list(HU=data1$HU),FUN=sum);udesejre <- udesejre[order(udesejre$HU),]
#Data fix
udesejre <- separate(data=udesejre,col=HU,into=c("H1","H2"),sep=" ")
udesejre$HU <- paste(udesejre$H2,udesejre$H1);udesejre <- udesejre[,4:3];udesejre <- udesejre[order(udesejre$HU),]
HUsejre <- c(hjemmesejre$x+udesejre$x)
samlet <- cbind.data.frame(hjemmesejre$HU,HUsejre)
colnames(samlet)[1] <- "Hold"
samlet <- separate(data=samlet,col=Hold,into=c("H1","H2"),sep=" ")
Y <- xtabs(samlet$HUsejre~H1+H2,samlet)
Y <- as.data.frame.matrix(Y)
#Danner designmatricen
UnikHold <- unique(data1$H)
UnikHold <- sort(UnikHold)
streak <- c(rep(1,length(UnikHold)))
#Danner ikke-tab-streak
for (hold in 1:length(UnikHold)){
  StreakSum = 0
  for (kamp in 1:length(data1[,1])) {
    if (data1$H[kamp] == UnikHold[hold]) {
      if (data1$HM[kamp] >= data1$UM[kamp]){
        StreakSum = StreakSum + 1
      } else {
        StreakSum = 0
        }
    } else if (data1$U[kamp] == UnikHold[hold]) {
      if (data1$HM[kamp] <= data1$UM[kamp]){
        StreakSum = StreakSum + 1
      } else {
        StreakSum = 0
      }
      }
    }
  streak[hold] = StreakSum
}
streak

GnsMal <- c(aggregate(data1$HM, by = list(H = data1$H),FUN = mean)[,2]+aggregate(data1$UM, by = list(U = data1$U),FUN = mean)[,2])/2
GnsMalInd <- c(aggregate(data1$UM, by = list(H = data1$H),FUN = mean)[,2]+aggregate(data1$HM, by = list(U = data1$U),FUN = mean)[,2])/2
GnsTilskuer <- c(aggregate(data1$Tilskuere, by = list(H = data1$H),FUN = mean)[,2]+aggregate(data1$Tilskuere, by = list(U = data1$U),FUN = mean)[,2])/1000
GnsBoldBes <- c(aggregate(dataUNan$boldb_h, by = list(H = dataUNan$H),FUN = mean)[,2]+aggregate(dataUNan$boldb_u, by = list(U = dataUNan$U),FUN = mean)[,2])
GnsSkud <- c(aggregate(dataUNan$skud_h, by = list(H = dataUNan$H),FUN = mean)[,2]+aggregate(dataUNan$skud_u, by = list(U = dataUNan$U),FUN = mean)[,2])/2
GnsSkudIndenfor <- c(aggregate(dataUNan$skud_inderfor_h, by = list(H = dataUNan$H),FUN = mean)[,2]+aggregate(dataUNan$skud_inderfor_u, by = list(U = dataUNan$U),FUN = mean)[,2])/2
GnsFrispark <- c(aggregate(dataUNan$frispark_h, by = list(H = dataUNan$H),FUN = mean)[,2]+aggregate(dataUNan$frispark_u, by = list(U = dataUNan$U),FUN = mean)[,2])/2
GnsHjorne <- c(aggregate(dataUNan$hjorne_h, by = list(H = dataUNan$H),FUN = mean)[,2]+aggregate(dataUNan$hjorne_u, by = list(U = dataUNan$U),FUN = mean)[,2])/2
#TeamRatings <- c(67,63,66,67,66,72,69,66,64,63,66,64,65,62,64,64)
TeamRatings <- c(66,63,65,68,64,71,70,66,63,67,64,64,64,64)-63
x <- as.data.frame.matrix(rbind(GnsHjorne,GnsMal,GnsMalInd,TeamRatings,GnsTilskuer,GnsBoldBes,GnsSkud,GnsSkudIndenfor,GnsFrispark))
#x <- as.matrix(rbind(GnsMal,GnsBoldBes,GnsSkud))
#Danner Antal-kampe-vektoren (r)
r <- xtabs(data1$Hsejr+data1$Usejr+data1$Uafgjort~H+U,data1)
r <- as.data.frame.matrix(r)
r <- r+t(r)
#Danner loglikelihood funktionen
#testværdier
#beta = c(1.6758,0.0269404)
#theta = 1.59384
#x <- cbind(c(0.23,4),c(0.67,29),c(0.51,7))
#Y <- cbind(c(0,13,12),c(3,0,3),c(5,13,0))
#Opskriver log-likelihoodfunktion

logl <- function(beta,theta,x){
  sum=0;
  for (i in 1:(dim(x)[2]-1)){
    for (j in (i+1):(dim(x)[2])){
      sum = sum +Y[i,j]*(t(x[,i])%*%beta-log(exp(t(x[,i])%*%beta)+theta*exp(t(x[,j])%*%beta)))+
                 Y[j,i]*(t(x[,j])%*%beta-log(exp(t(x[,j])%*%beta)+theta*exp(t(x[,i])%*%beta)))+
                 (r[i,j]-Y[i,j]-Y[j,i])*(log(theta^2-1)+t(x[,i])%*%beta+t(x[,j])%*%beta-log(exp(t(x[,i])%*%beta)+
                 theta*exp(t(x[,j])%*%beta))-log(exp(t(x[,j])%*%beta)+theta*exp(t(x[,i])%*%beta)))
    }
 }
return(sum)
}
logl(beta,theta,x)
#Definerer dldb til at være første afledte ift. beta
#i=1;j=1;sum=0;
#sum = c(0,0)
#sum <- as.vector(sum)
#db <- as.vector(db)
db <- function(beta,theta,x){
#  sum = c(0,0);
  for(i in 1:(dim(x)[2]-1)){
    for(j in (i+1):dim(x)[2]) {
      sum = sum + (c(r[i,j]-Y[i,j])*(-(theta*exp(t(x[,i])%*%beta))/(exp(t(x[,j])%*%beta)+theta*exp(t(x[,i])%*%beta)))
          +c(r[i,j]-Y[j,i])*((theta*exp(t(x[,j])%*%beta))/(exp(t(x[,i])%*%beta)+theta*exp(t(x[,j])%*%beta)))
          )*(x[,i]-x[,j])
    }
  }
  return(sum)
}
db(beta,theta,x)#-7.809544e-06 -4.273634e-04
#Tænker det egentligt er fint, at dldb giver 0,0?
#Det betyder vel bare at det givne data er ved et stationert punkt?
i=1;j=1;sum=0;
dtheta <- function(beta,theta,x) {
  sum=0;
  for(i in 1:(dim(x)[2]-1)) {
    for(j in (i+1):dim(x)[2]) {
      sum = sum + ((r[i,j]-Y[i,j]-Y[j,i])*(2*theta/(theta^2-1))
                + (r[i,j]-Y[i,j])*(-(exp(t(x[,i])%*%beta)/(theta*exp(t(x[,i])%*%beta)+exp(t(x[,j])%*%beta))))
                + (r[i,j]-Y[j,i])*(-(exp(t(x[,j])%*%beta)/(exp(t(x[,i])%*%beta)+theta*exp(t(x[,j])%*%beta))))
      )
    }
  }
  return(sum)
}

dtheta2 <- function(beta,theta,x) {
  sum=0;
  for(i in 1:(dim(x)[2]-1)) {
    for(j in (i+1):dim(x)[2]) {
      sum = sum + ((r[i,j]-Y[i,j]-Y[j,i])*(-(2*(theta^2+1))/(theta^2-1)^2)
                +(r[i,j]-Y[i,j])*(exp(2*t(x[,i])%*%beta)/(theta*exp(t(x[,i])%*%beta)+exp(t(x[,j])%*%beta))^2)
                +(r[i,j]-Y[j,i])*(exp(2*t(x[,j])%*%beta)/(exp(t(x[,i])%*%beta)+theta*exp(t(x[,j])%*%beta))^2)
      )
    }
  }
  return(sum)
}

db2 <- function(beta,theta,x){
  sum = 0;
  for(i in 1:(dim(x)[2]-1)){
    for(j in (i+1):dim(x)[2]) {
      sum = sum + (as.numeric((((-(r[i,j]-Y[i,j])/(theta*exp(t(x[,i])%*%beta)+exp(t(x[,j])%*%beta))^2)
                -(r[i,j]-Y[j,i])/(exp(t(x[,i])%*%beta)+theta*exp(t(x[,j])%*%beta))^2))
                *(exp(t(x[,i])%*%beta+t(x[,j])%*%beta)*theta))*((x[,i]-x[,j])%*%t(x[,i]-x[,j]))
      )
    }
  }
  return(sum)
}

dbt <- function(beta,theta,x){
  sum = 0;
  for(i in 1:(dim(x)[2]-1)){
    for(j in (i+1):dim(x)[2]) {
      sum = sum + ((as.numeric((-(r[i,j]-Y[i,j])/(theta*exp(t(x[,i])%*%beta)+exp(t(x[,j])%*%beta))^2)+
                  ((r[i,j]-Y[j,i])/(exp(t(x[,i])%*%beta)+theta*exp(t(x[,j])%*%beta))^2))*
                 (exp(t(x[,i])%*%beta+t(x[,j])%*%beta))*(x[,i]-x[,j]))
      )
      }
  }
  return(sum)
}

#iterationsvektoren:
#Y = Y*100;r = r*100;
#Y = Y/100;r = r/100;
ite = as.matrix(c(rep(0.1,dim(x)[1]),1.1));counter=0;val=1;
while(abs(val)>0.0000001){
beta = c(ite[1:(dim(x)[1])]);
theta=ite[dim(x)[1]+1];
#Danner gradienten
a12 = as.matrix(db(beta,theta,x));
grad = rbind(a12,dtheta(beta,theta,x));
#Opsriver blokkene
A = db2(beta,theta,x);B = as.matrix(dbt(beta,theta,x));C = t(as.matrix(dbt(beta,theta,x)));D = dtheta2(beta,theta,x);
#Kombinerer blokkene til informationen
inf = cbind(A,B);inf = rbind(inf,c(C,D));inf=-inf;
temp = ite;
ite = ite + Inverse(inf)%*%grad*(1/2);
val = sum(temp-ite);
logl(beta,theta,x)
counter = counter +1;
}
KV <- inv(inf)
U <- sqrt(diag(KV));U
c(beta,theta) - 1.96*U
sort(exp(t(x)%*%beta))
styrker <- exp(t(x)%*%beta)
names(x) <- names(Y)

#LRT
pchisq(-2*(-269.023+243.6382),9,lower = F)

#Sandsynlighederne
ssh <- function(beta,theta,x,i,j){
  VTU=0;
      VTU = c((styrker[i])/(styrker[i]+theta*styrker[j]),
            ((styrker[j])/(styrker[i]*theta+styrker[j])),
            ((styrker[i]*styrker[j]*(theta^2-1))/((styrker[i]+theta*styrker[j])*(styrker[i]*theta+styrker[j])))
        )
  return(VTU)
}
styrker
UnikHold
#Kummulerede sandsynligheder
r[5,12]*ssh(beta,theta,x,5,13)

#Dynamisk model
startrunde = 10;
Fejl <- c(rep(0,length(unique(data1$runde))-9))
for (round in startrunde:length(unique(data1$runde))) {
data1 <- data[which((data$dato>="2018-07-13") & (data$dato <= "2019-05-26")),]
data1 <- data1[which(data1$runde<=round),]
dataUNan <- na.omit(data1)
data1$HU <- paste(data1$H,data1$U)
UnikSammenligner <- unique(data1$HU)
#Udregner antal hjemmesejre og udesejre i kombinationerne
hjemmesejre <- aggregate(data1$Hsejr, by = list(HU=data1$HU),FUN=sum);hjemmesejre <- hjemmesejre[order(hjemmesejre$HU),]
udesejre <- aggregate(data1$Usejr, by = list(HU=data1$HU),FUN=sum);udesejre <- udesejre[order(udesejre$HU),]
#Data fix
udesejre <- separate(data=udesejre,col=HU,into=c("H1","H2"),sep=" ")
udesejre$HU <- paste(udesejre$H2,udesejre$H1);udesejre <- udesejre[,4:3];udesejre <- udesejre[order(udesejre$HU),]
HUsejre <- c(hjemmesejre$x+udesejre$x)
samlet <- cbind.data.frame(hjemmesejre$HU,HUsejre)
colnames(samlet)[1] <- "Hold"
samlet <- separate(data=samlet,col=Hold,into=c("H1","H2"),sep=" ")
Y <- xtabs(samlet$HUsejre~H1+H2,samlet)
Y <- as.data.frame.matrix(Y)
#Designmatrice
GnsMal <- c(aggregate(data1$HM, by = list(H = data1$H),FUN = mean)[,2]+aggregate(data1$UM, by = list(U = data1$U),FUN = mean)[,2])/2
GnsMalInd <- c(aggregate(data1$UM, by = list(H = data1$H),FUN = mean)[,2]+aggregate(data1$HM, by = list(U = data1$U),FUN = mean)[,2])/2
GnsTilskuer <- c(aggregate(data1$Tilskuere, by = list(H = data1$H),FUN = mean)[,2]+aggregate(data1$Tilskuere, by = list(U = data1$U),FUN = mean)[,2])/1000
GnsBoldBes <- c(aggregate(dataUNan$boldb_h, by = list(H = dataUNan$H),FUN = mean)[,2]+aggregate(dataUNan$boldb_u, by = list(U = dataUNan$U),FUN = mean)[,2])
GnsSkud <- c(aggregate(dataUNan$skud_h, by = list(H = dataUNan$H),FUN = mean)[,2]+aggregate(dataUNan$skud_u, by = list(U = dataUNan$U),FUN = mean)[,2])/2
GnsSkudIndenfor <- c(aggregate(dataUNan$skud_inderfor_h, by = list(H = dataUNan$H),FUN = mean)[,2]+aggregate(dataUNan$skud_inderfor_u, by = list(U = dataUNan$U),FUN = mean)[,2])/2
GnsFrispark <- c(aggregate(dataUNan$frispark_h, by = list(H = dataUNan$H),FUN = mean)[,2]+aggregate(dataUNan$frispark_u, by = list(U = dataUNan$U),FUN = mean)[,2])/2
GnsHjorne <- c(aggregate(dataUNan$hjorne_h, by = list(H = dataUNan$H),FUN = mean)[,2]+aggregate(dataUNan$hjorne_u, by = list(U = dataUNan$U),FUN = mean)[,2])/2
#TeamRatings <- c(67,63,66,67,66,72,69,66,64,63,66,64,65,62,64,64)
TeamRatings <- c(66,63,65,68,64,71,70,66,63,67,64,64,64,64)-63
for (hold in 1:length(UnikHold)){
  StreakSum = 0
  for (kamp in 1:length(data1[,1])) {
    if (data1$H[kamp] == UnikHold[hold]) {
      if (data1$HM[kamp] >= data1$UM[kamp]){
        StreakSum = StreakSum + 1
      } else {
        StreakSum = 0
      }
    } else if (data1$U[kamp] == UnikHold[hold]) {
      if (data1$HM[kamp] <= data1$UM[kamp]){
        StreakSum = StreakSum + 1
      } else {
        StreakSum = 0
      }
    }
  }
  streak[hold] = StreakSum
}
x <- as.data.frame.matrix(rbind(streak,GnsHjorne,GnsMal,GnsMalInd,TeamRatings,GnsTilskuer,GnsBoldBes,GnsSkud,GnsSkudIndenfor,GnsFrispark))
#x <- as.matrix(rbind(GnsMal,GnsBoldBes,GnsSkud))
#Danner Antal-kampe-vektoren (r)
r <- xtabs(data1$Hsejr+data1$Usejr+data1$Uafgjort~H+U,data1)
r <- as.data.frame.matrix(r)
r <- r+t(r)
#Estimerer parametre
ite = as.matrix(c(rep(0,dim(x)[1]),1.1));counter=0;val=1;
while(abs(val)>0.0000001){
  beta = c(ite[1:(dim(x)[1])]);
  theta=ite[dim(x)[1]+1];
  #Danner gradienten
  a12 = as.matrix(db(beta,theta,x));
  grad = rbind(a12,dtheta(beta,theta,x));
  #Opsriver blokkene
  A = db2(beta,theta,x);B = as.matrix(dbt(beta,theta,x));C = t(as.matrix(dbt(beta,theta,x)));D = dtheta2(beta,theta,x);
  #Kombinerer blokkene til informationen
  inf = cbind(A,B);inf = rbind(inf,c(C,D));inf=-inf;
  temp = ite;
  ite = ite + Inverse(inf)%*%grad*(1/2);
  val = sum(temp-ite);
  logl(beta,theta,x)
  counter = counter +1;
}
#Evaluerer modellen, ift. sandsynligheder for udfald mod de observerede udfald i næste runde
#Finder samtlige parvise sandsynligheder for ALLE sammenligninger
counter = 1;KumSSH <- matrix(NA, nrow = (factorial(length(UnikHold))/(factorial((length(UnikHold)-2)))), ncol = 5)
for (hold1 in 1:length(UnikHold)) {
  for (hold2 in 1:length(UnikHold)) {
      if (hold1 != hold2){
      KumSSH[counter,] <- cbind(ssh(beta,theta,x,hold1,hold2)[1],ssh(beta,theta,x,hold1,hold2)[2],ssh(beta,theta,x,hold1,hold2)[3],UnikHold[hold1],UnikHold[hold2])
      counter = counter +1
      }
    }
}
#Fikser data og sætter Holdene sammen i én column
KumSSH <- as.data.frame.matrix(KumSSH)
names(KumSSH) <- c("H","U","Uafgjort","H1","H2")
KumSSH$H1H2 <- paste(KumSSH$H1,KumSSH$H2)
KumSSH <- KumSSH[,c(1:3,6)]
KumSSH[,1]<-as.numeric(paste(KumSSH[,1]));KumSSH[,2] <- as.numeric(paste(KumSSH[,2]));KumSSH[,3] <- as.numeric(paste(KumSSH[,3]));
#Finder næste rundes kampe for at evaluere
data1 <- data[which((data$runde==round+1) & (data$dato>="2018-07-13") & (data$dato <= "2019-05-26")),]
data1$HU <- paste(data1$H,data1$U)
#Finder dertilhørende ssh
KumSSH <- KumSSH[KumSSH$H1H2 %in% data1$HU,];

Observationer <- as.data.frame.matrix(cbind(data1$Hsejr,data1$Usejr,data1$Uafgjort,data1$HU));
Observationer <- Observationer[order(Observationer$V4),];Observationer[,1] <- as.numeric(paste(Observationer[,1]));Observationer[,2] <- as.numeric(paste(Observationer[,2]));Observationer[,3] <- as.numeric(paste(Observationer[,3]));
Fejl[round-9] = sum(Observationer[,1:3]-(KumSSH[,1:3]*Observationer[,1:3]))

}
Fejl
