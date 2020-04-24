#git add Probabilities.R #git commit -m "Navn på ændring" #git push #git pull
rm(list=ls())
library(matlib) #Til vektor/matrix regning
library(blockmatrix) #matrixopsætning
library(tidyr) #Til data transformation

setwd("C:/Users/lucas/Desktop/Odd")
#Henter og verificerer data
data <- read.table("kampe_r1.csv",header=T,sep=",")
head(data)
str(data)
#fikser datatypes
data$H <- as.character(data$H)
data$U <- as.character(data$U)
data$dato <- as.Date(data$dato, format = "%m/%d/%Y")
#Henter 1.5 sæson ud:
data1 <- data[which(data$dato>="2018-07-13"),]
#Finder holdene der er i Superligaen i indeværende sæson
data1 <- na.omit(data1)
attach(data1)
#sætter Y-kontingenstabellen op:
#hjemmesejre
aggregate(data1$Hsejr,by=list(H=data1$H),FUN=sum)
#udesejre
aggregate(data1$Usejr,by=list(U=data1$U),FUN=sum)
#Samler strengen af hjemme mod ude
data1$HU <- paste(data1$H,data1$U)
#Udregner antal hjemmesejre og udesejre i kombinationerne
hjemmesejre <- aggregate(data1$Hsejr, by = list(HU=data1$HU),FUN=sum);hjemmesejre <- hjemmesejre[order(hjemmesejre$HU),]
udesejre <- aggregate(data1$Usejr, by = list(HU=data1$HU),FUN=sum);udesejre <- udesejre[order(udesejre$HU),]
#Data fix
HUsejre <- c(hjemmesejre$x+udesejre$x)
samlet <- cbind.data.frame(hjemmesejre$HU,HUsejre)
colnames(samlet)[1] <- "Hold"
samlet <- separate(data=samlet,col=Hold,into=c("H1","H2"),sep=" ")
Y <- xtabs(samlet$HUsejre~H1+H2,samlet)
Y <- as.data.frame.matrix(Y)
#Danner designmatricen
UnikHold <- unique(data1$H)
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

GnsMal <- c(aggregate(data1$HM, by = list(H = data1$H),FUN = mean)[,2]+aggregate(data1$UM, by = list(U = data1$U),FUN = mean)[,2])
GnsTilskuer <- c(aggregate(data1$Tilskuere, by = list(H = data1$H),FUN = mean)[,2]+aggregate(data1$Tilskuere, by = list(U = data1$U),FUN = mean)[,2])/1000
GnsBoldBes <- c(aggregate(data1$boldb_h, by = list(H = data1$H),FUN = mean)[,2]+aggregate(data1$boldb_u, by = list(U = data1$U),FUN = mean)[,2])
GnsSkud <- c(aggregate(data1$skud_h, by = list(H = data1$H),FUN = mean)[,2]+aggregate(data1$skud_u, by = list(U = data1$U),FUN = mean)[,2])
GnsSkudIndenfor <- c(aggregate(data1$skud_inderfor_h, by = list(H = data1$H),FUN = mean)[,2]+aggregate(data1$skud_inderfor_u, by = list(U = data1$U),FUN = mean)[,2])
GnsFrispark <- c(aggregate(data1$frispark_h, by = list(H = data1$H),FUN = mean)[,2]+aggregate(data1$frispark_u, by = list(U = data1$U),FUN = mean)[,2])

x <- as.matrix(rbind(GnsMal,GnsTilskuer,streak,GnsBoldBes,GnsSkud,GnsSkudIndenfor,GnsFrispark))
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
i=1;j=1;sum=0;
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
i=1;j=1;sum=0;
sum = c(0,0)
sum <- as.vector(sum)
#db <- as.vector(db)
db <- function(beta,theta,x){
  sum = c(0,0);
  for(i in 1:(dim(x)[2]-1)){
    for(j in (i+1):dim(x)[2]) {
      sum = sum + ((r[i,j]-Y[i,j])*(-(theta*exp(t(x[,i])%*%beta))/(exp(t(x[,j])%*%beta)+theta*exp(t(x[,i])%*%beta)))
          +(r[i,j]-Y[j,i])*((theta*exp(t(x[,j])%*%beta))/(exp(t(x[,i])%*%beta)+theta*exp(t(x[,j])%*%beta)))
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
ite = as.matrix(c(rep(.1,dim(x)[1]),1.1));counter=0;val=1;
while(abs(val)>0.0000001){
beta = c(ite[1:(dim(x)[1])]);theta=ite[dim(x)[1]+1];
#Danner gradienten
a12 = as.matrix(db(beta,theta,x));
grad = rbind(a12,dtheta(beta,theta,x));
#Opsriver blokkene
A = db2(beta,theta,x);B = as.matrix(dbt(beta,theta,x));C = t(as.matrix(dbt(beta,theta,x)));D = dtheta2(beta,theta,x);
#Kombinderer blokkene til informationen
inf = cbind(A,B);inf = rbind(inf,c(C,D));inf=-inf;
temp = ite;
ite = ite + Inverse(inf)%*%grad*(1/2);
val = sum(temp-ite);
logl(beta,theta,x)
counter = counter +1;
}
KV <- inv(inf)
U <- sqrt(diag(KV));U