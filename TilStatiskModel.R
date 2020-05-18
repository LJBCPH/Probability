library(matlib) #Til vektor/matrix regning
library(MASS) # Til invers af numerisk problematiske matricer
library(blockmatrix) #matrixopsÃ¦tning
library(tidyr) #Til data transformation
library(xtable) #Til Latex table
library(BTSoccer)
library(ggplot2) #plots
library(psych) #pairs.panel
library(SSLASSO)
library(KernSmooth)
library(directlabels)
rm(list=ls())
setwd("C:/Users/Victo/Desktop/bachelor/kode")
data <- read.table("Kampe_r1.csv",header=T,sep=",")
data <- read.table(file.choose(),header=T,sep=",")
CreateMatrixes1 <- function(data,StartDate,EndDate,round) {
  #fikser datatypes
  data$H <- as.character(data$H)
  data$U <- as.character(data$U)
  data$dato <- as.Date(data$dato, format = "%m/%d/%Y")
  #Henter 1.5 sæson ud:
  data1 <- data[which((data$dato>=StartDate) & (data$dato <= EndDate)),]
  StartDate <- "2015-07-17";EndDate <- "2016-05-29"
  #Finder holdene der er i Superligaen i indeværende sæson
  dataUNan <- na.omit(data1)
  #detach(data1)
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
  GnsMal <- c(aggregate(data1$HM, by = list(H = data1$H),FUN = sum)[,2]+aggregate(data1$UM, by = list(U = data1$U),FUN = sum)[,2])
  GnsMalInd <- c(aggregate(data1$UM, by = list(H = data1$H),FUN = sum)[,2]+aggregate(data1$HM, by = list(U = data1$U),FUN = sum)[,2])
  GnsTilskuer <- c(aggregate(data1$Tilskuere, by = list(H = data1$H),FUN = mean)[,2]+aggregate(data1$Tilskuere, by = list(U = data1$U),FUN = mean)[,2])/1000
  GnsBoldBes <- c(aggregate(dataUNan$boldb_h, by = list(H = dataUNan$H),FUN = mean)[,2]+aggregate(dataUNan$boldb_u, by = list(U = dataUNan$U),FUN = mean)[,2])
  GnsSkud <- c(aggregate(dataUNan$skud_h, by = list(H = dataUNan$H),FUN = mean)[,2]+aggregate(dataUNan$skud_u, by = list(U = dataUNan$U),FUN = mean)[,2])/2
  GnsSkudIndenfor <- c(aggregate(dataUNan$skudindenfor_h, by = list(H = dataUNan$H),FUN = mean)[,2]+aggregate(dataUNan$skudindenfor_u, by = list(U = dataUNan$U),FUN = mean)[,2])/2
  GnsFrispark <- c(aggregate(dataUNan$frispark_h, by = list(H = dataUNan$H),FUN = mean)[,2]+aggregate(dataUNan$frispark_u, by = list(U = dataUNan$U),FUN = mean)[,2])/2
  GnsHjorne <- c(aggregate(dataUNan$hjorne_h, by = list(H = dataUNan$H),FUN = mean)[,2]+aggregate(dataUNan$hjorne_u, by = list(U = dataUNan$U),FUN = mean)[,2])/2
  GnsOffside <- c(aggregate(dataUNan$offside_h, by = list(H = dataUNan$H),FUN = mean)[,2]+aggregate(dataUNan$offside_u, by = list(U = dataUNan$U),FUN = mean)[,2])/2
  #TeamRatings <- c(67,63,66,67,66,72,69,66,64,63,66,64,65,62,64,64)
  #TeamRatings <- c(65,65,66,65,71,69,63,63,64,66,66,63)
  TeamRatings <- c(65,67,65,71,69,64,64,64,66,65,63,66)
  x <- as.data.frame.matrix(rbind(TeamRatings,GnsHjorne,GnsOffside,GnsMal,GnsMalInd,GnsTilskuer,GnsBoldBes,GnsSkud,GnsSkudIndenfor,GnsFrispark))
  x <- t(standard(t(x)))
  x <- as.data.frame.matrix(x)
  names(x) <- names(Y)
  #x <- as.matrix(rbind(GnsMal,GnsBoldBes,GnsSkud))
  #Danner Antal-kampe-vektoren (r)
  r <- xtabs(data1$Hsejr+data1$Usejr+data1$Uafgjort~H+U,data1)
  r <- as.data.frame.matrix(r)
  r <- r+t(r)
  Matrixes <- list("DesignMatrix" = x,"KontingensTabel" = Y,"SamledeKampe" = r)
  return(Matrixes)
}
str(data)
BTFunktioner1 <- function(beta,theta,x,Y,r) {
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

  db <- function(beta,theta,x){
    sum=0;
    for(i in 1:(dim(x)[2]-1)){
      for(j in (i+1):dim(x)[2]) {
        sum = sum + (c(r[i,j]-Y[i,j])*(-(theta*exp(t(x[,i])%*%beta))/(exp(t(x[,j])%*%beta)+theta*exp(t(x[,i])%*%beta)))
                     +c(r[i,j]-Y[j,i])*((theta*exp(t(x[,j])%*%beta))/(exp(t(x[,i])%*%beta)+theta*exp(t(x[,j])%*%beta)))
        )*(x[,i]-x[,j])
      }
    }
    return(sum)
  }
  dtheta <- function(beta,theta,x) {
    sum = 0;
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
    sum = 0;
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
    sum=0;
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
  Funktions <- list("loglike" = logl,"dlbeta" = db,"dltheta" = dtheta,"dl2xtheta" = dtheta2,"dl2xbeta"= db2,"dlbetatheta" = dbt)
  return(Funktions)
}

NR1 <- function(x,f,Beta,SBeta,lambda=0,c=10^(-10),lamblimit=10^(-5)) {
  if(missing(SBeta)){
  ite = as.matrix(c(rep(0.1,dim(x)[1]),1.1));counter=0;val=1;
  }else{
    ite = as.matrix(c(SBeta,1.1));counter=0;val=1;
    x <- x[which(rownames(x)%in%names(SBeta)),]
  }
  while(abs(val)>10^(-8)){
    if(missing(Beta)){
    beta = c(ite[1:(dim(x)[1])]);
    } else {
    beta = Beta
    }

    theta=ite[dim(x)[1]+1];
    a12 = as.matrix(f$dlbeta(beta,theta,x)-lambda*(beta/sqrt(beta^2+c^2)))
    grad = rbind(a12,f$dltheta(beta,theta,x));
    A = f$dl2xbeta(beta,theta,x)-lambda*(c^2)/((beta^2+c^2)^(3/2));B = as.matrix(f$dlbetatheta(beta,theta,x));C = t(as.matrix(f$dlbetatheta(beta,theta,x)));D = f$dl2xtheta(beta,theta,x);
    inf = cbind(A,B);inf = rbind(inf,c(C,D));inf=-inf;
    temp = ite;
    i=0;foundstep=FALSE;itetemp=0;thetatest=0;betatest=c(rep(0,length(beta)));
    while(i < 150&foundstep==FALSE){
      i=i+1
      w=1/i;
      itetemp=ite + Inverse(inf)%*%grad*w
      betatest = c(itetemp[1:(dim(x)[1])])
      thetatest =itetemp[dim(x)[1]+1]
      if(f$loglike(betatest,thetatest,x)-lambda*sum(sqrt(betatest^2+c^2)) > f$loglike(beta,theta,x)-lambda*sum(sqrt(beta^2+c^2)))
      {
        foundstep = TRUE
      }
    }
    if(foundstep==FALSE){
      cat("\n fandt ingen skridtlænge: breakAAAAA")
      cat("\n logl :",f$loglike(beta,theta,x)-lambda*sum(sqrt(beta^2+c^2)))
      styrker <- exp(t(x)%*%beta)
      rownames(styrker)=na.omit(names(x));names(beta)=na.omit(rownames(x))
      KV <- inv(inf)
      U <- sqrt(diag(KV))
      Values = list("beta" = beta, "theta" = theta,"Styrker" = styrker,"sd" = U,"KV"=KV)
      return(Values)
      break
      }
    ite = ite + Inverse(inf)%*%grad*w;
    val = sum(temp-ite);
    for(i in length(beta):1){
      if((abs(ite[i])<lamblimit || sign(ite[i])!=sign(temp[i]))&&counter>2){
        ite <- ite[-i]
        x <- x[-i,]
        beta <- beta[-i]
        cat("\nFjernet Koefficient nr: ",i)
      }
    }
    cat(f$loglike(beta,theta,x),"   ",counter,"\n",beta,"\n",theta,"\n")
    counter = counter +1;
  }
  cat("logl :",f$loglike(beta,theta,x)-lambda*sum(sqrt(beta^2+c^2)))
  styrker <- exp(t(x)%*%beta)
  rownames(styrker)=na.omit(names(x));names(beta)=na.omit(rownames(x))
  KV <- inv(inf)
  U <- sqrt(diag(KV))
  Values = list("beta" = beta, "theta" = theta,"Styrker" = styrker,"sd" = U,"KV"=KV)
  return(Values)
}

Sandsynligheder1 <- function(styrker,i,j){
  VTU=0;
  VTU = c((styrker[i])/(styrker[i]+theta*styrker[j]),
          ((styrker[j])/(styrker[i]*theta+styrker[j])),
          ((styrker[i]*styrker[j]*(theta^2-1))/((styrker[i]+theta*styrker[j])*(styrker[i]*theta+styrker[j])))
  )
  return(VTU)
}
m <- CreateMatrixes1(data,"2015-07-17","2016-05-29",0)
x <- m$DesignMatrix;Y<-m$KontingensTabel;r<-m$SamledeKampe
data
f <- BTFunktioner1(beta,theta,x,Y,r)
n <- NR1(SBeta=beta,x,f,lambda=100);beta <- n$beta;theta<-n$theta;beta
beta0 <- beta
sum(abs(beta))
names(beta)=rownames(x)
n$sd
betas <- beta
b0 <- c(rep(0,length(n$beta)))
lrh0 <- f$loglike(b0,1.666556,x);lrh0
lrmle <- -190.644
round(-2*(lrh0-lrmle),3)

round(n$beta,6)
round(n$theta,6)
round(n$sd,6)
#standardfejl for styrker
varbeta <- n$KV[-11,-11]#varians på betaer
sqrt(diag(varbeta))#Standardfejl på betaer
t(x[,1])%*%beta #log(AGFs styrke)
vlpi1<- t(x[,1])%*%varbeta%*%x[,1]#Varians på log(AGF)
sqrt(diag(vlpi1))#Standardfejl på log(AGF)
piagf <- exp(x[,1]%*%beta);piagf#AGFs styrke
varagf <- exp(t(x[,1])%*%beta)%*%(t(x[,1])%*%varbeta%*%x[,1])%*%exp(t(x[,1])%*%beta);varagf#Varians på AGFs styrke
sfagf <-sqrt(varagf);sfagf#standardfejl på AGFs styrke


#Styrker og standardfejl ift Hobro:
#x <- x-x[,7]
#sqrt(diag(n$KV))
varbeta <- n$KV[-11,-11]#varians på betaer
sfb <- sqrt(diag(varbeta));names(sfb)=names(beta)#Standardfejl på betaer
round(beta,3)
round(sfb,3)
#t(x[,1])%*%beta #log(AGFs styrke)
#vlpi1<- t(x[,1])%*%varbeta%*%x[,1]#Varians på log(AGF)
#sfls <-sqrt(diag(vlpi1));sfls#Standardfejl på log(AGF)
#varpi <- exp(t(x[,1])%*%beta)%*%(t(x[,1])%*%varbeta%*%x[,1])%*%exp(t(x[,1])%*%beta);varagf#Varians på AGFs styrke
#sfagf <-sqrt(varagf);sfagf#standardfejl på AGFs styrke
#exp(t(x[,1])%*%beta)*sfls

x <- x-x[,7];piH <- c(rep(1,12));varpiH <- c(rep(1,12));varbeta <- n$KV[-11,-11];
for (i in 1:12){
  piH[i]=exp(x[,i]%*%beta)
}
for (i in 1:12){
  varpiH[i]= exp(t(x[,i])%*%beta)%*%(t(x[,i])%*%varbeta%*%x[,i])%*%exp(t(x[,i])%*%beta)
}
sfpiH <- sqrt(varpiH);
names(varpiH)=names(x);names(sfpiH)=names(x);names(piH)=names(x)
round(piH,3)#styrker
round(varpiH,3)#varians
round(sfpiH,3)#standardfejl
###point

UnikHold=names(piH);counter = 1;KumSSH <- matrix(NA, nrow = (factorial(length(UnikHold))/(factorial((length(UnikHold)-2)))), ncol = 5)
for (hold1 in 1:length(UnikHold)) {
  for (hold2 in 1:length(UnikHold)) {
    if (hold1 != hold2){
      KumSSH[counter,] <- cbind(Sandsynligheder1(piH,hold1,hold2)[1],Sandsynligheder1(piH,hold1,hold2)[2],Sandsynligheder1(piH,hold1,hold2)[3],UnikHold[hold1],UnikHold[hold2])
      counter = counter +1
    }
  }
}
KumSSH[which(KumSSH[,4]=="AaB"),][1]*3+KumSSH[which(KumSSH[,4]=="AaB"),][3]*1
Point1 <- 3*(KumSSH[which(KumSSH[,4]=="AaB"),][1]*3+KumSSH[which(KumSSH[,4]=="AaB"),][3]*1)
point<-c(rep(0,12));
for(j in 1:length(UnikHold)){
for (i in 1:(length(UnikHold)-1)){
  point[j]=point[j]+3*(3*as.numeric(KumSSH[which(KumSSH[,4]==UnikHold[j]),][i,1])+as.numeric(KumSSH[which(KumSSH[,4]==UnikHold[j]),][i,3])*1)
 }
}
names(point)=names(x)
sum(point)
beta
UnikHold[1]
as.numeric(KumSSH[which(KumSSH[,4]==UnikHold[1]),][,1])
#Krydsvalidering
dataa <- data[which((data$dato>=StartDate) & (data$dato <= EndDate)),]
data3 <- dataa[which(dataa$runde>3),]
StartDate <- "2015-07-17";EndDate <- "2016-05-29"
length(data3[,1])
dim(as.data.frame(split(data3,sample(1:6,33,replace=T))))

ttt <- sample(unique(data3$runde),30,replace=F)
q1 <- data3[which(data3$runde%in%ttt[1:5]),];q2 <- data3[which(data3$runde%in%ttt[6:10]),]
q3 <- data3[which(data3$runde%in%ttt[11:15]),];q4 <- data3[which(data3$runde%in%ttt[16:20]),]
q5 <- data3[which(data3$runde%in%ttt[21:25]),];q6 <- data3[which(data3$runde%in%ttt[26:30]),]
Q <- list(q1,q2,q3,q4,q5,q6)
################################################
5/0.01
lambdatest=0;
for(i in 1:500){
  lambdatest[i]=exp(i*0.0001)
}

lambdatest=lambdatest-1
KrydsVal <- cbind(c(4,8),c(9,13),c(14,18),c(19,23),c(24,28),c(29,33))
a = 0;b=0;aa=0;bb=0;BetaAvgerage=0;kryds=0;Kryds=0;
NumbBeta=0;beta=beta0;AA <- as.matrix(rbind(c(0,0,0)));BB <- as.matrix(rbind(c(0,0,0)));NumbBeta = 0;BetaAvg <- vector("list",length(lambdatest))
for(lambda in lambdatest){
  cat("\nTESTER NU LAMBDA = ",lambda)
  cat("\nTESTER NU LAMBDA = ",lambda)
  cat("\nTESTER NU LAMBDA = ",lambda,"\n")
  NumbBeta=NumbBeta+1
  BetaAvgerage=0
    for(j in 1:6){
      a = 0;b=0;
      m <- CreateMatrixes1(data3[which((data3$runde<KrydsVal[1,j])|(data3$runde>KrydsVal[2,j])),],"2015-07-17","2016-05-29",0)
      x <- m$DesignMatrix;Y<-m$KontingensTabel;r<-m$SamledeKampe
      f <- BTFunktioner1(beta,theta,x,Y,r)
      n <- NR1(SBeta=beta,x,f,lambda=lambda);beta <- n$beta;theta<-n$theta;beta
      data2=data3[which((data3$runde>KrydsVal[1,j])&(data3$runde<KrydsVal[2,j])),]
      x <- x[which(rownames(x)%in%names(beta)),]
      #beta0 <- beta
      sum(r)
for (i in 1:12){
  pi[i]=exp(x[,i]%*%beta)
}
UnikHold=names(x);counter = 1;KumSSH <- matrix(NA, nrow = (factorial(length(UnikHold))/(factorial((length(UnikHold)-2)))), ncol = 5)
for (hold1 in 1:length(UnikHold)) {
  for (hold2 in 1:length(UnikHold)) {
    if (hold1 != hold2){
      KumSSH[counter,] <- cbind(Sandsynligheder1(pi,hold1,hold2)[1],Sandsynligheder1(pi,hold1,hold2)[2],Sandsynligheder1(pi,hold1,hold2)[3],UnikHold[hold1],UnikHold[hold2])
      counter = counter +1
    }
  }
}
for(k in KrydsVal[1,j]:KrydsVal[2,j]){
  data2=data3[which((data3$runde==k)),]

  KumSSH <- as.data.frame.matrix(KumSSH) #kummuleret ssh
  names(KumSSH) <- c("H","U","Uafgjort","H1","H2")
  KumSSH$H1H2 <- paste(KumSSH$H1,KumSSH$H2)
  data2$HU <- paste(data2$H,data2$U)
  ssh <- KumSSH[KumSSH$H1H2 %in% data2$HU,]
  ssh$H <- as.numeric(as.character(ssh$H));ssh$U <- as.numeric(as.character(ssh$U));ssh$Uafgjort <- as.numeric(as.character(ssh$Uafgjort))
  ssh <- ssh[order(ssh$H1H2),]
  data2 <- data2[order(data2$HU),]
  as.data.frame(lapply((cbind(data2$Hsejr,data2$Usejr,data2$Uafgjort)-cbind(ssh$H,ssh$U,ssh$Uafgjort))^2, function(y) sum(y)))
  A <- c(rowSums(as.matrix((cbind(data2$Hsejr,data2$Usejr,data2$Uafgjort)-cbind(ssh$H,ssh$U,ssh$Uafgjort))^2))) #(Y-Yhat)^2
  a <- c(append(a,A,after= length(a)))
  B <- c(rowSums((cbind(data2$Hsejr*log(ssh$H),data2$Usejr*log(ssh$U),data2$Uafgjort*log(ssh$Uafgjort))))) #log(Yhat)
  b <- c(append(b,B,after = length(b)))
  #      length(beta0) <- length(BetaAvgerage)
}
BetaAvgerage <- BetaAvgerage+beta
AA <- cbind(c(append(AA[,1],a,after=length(AA[,1]))),c(append(AA[,2],rep(lambda,length(a)),after=length(AA[,1]))))
BB <- cbind(c(append(BB[,1],b,after=length(BB[,1]))),c(append(BB[,2],rep(lambda,length(b)),after=length(BB[,1]))))
#BetaAvg <- cbind(c(append(BetaAvg[,1],BetaAvg/6,after=length(BetaAvg[,1]))),c(append(BetaAvg[,2],rep(Lambda,length(beta)),after=length(BetaAvg[,1]))))
BetaAvg[[NumbBeta]] <- cbind(c(BetaAvgerage/6),c(rep(lambda,length(BetaAvgerage))))
cat("\n\n\n",lambda,"\n\n\n")
}

}
#########################################################################################
m1 <- rbind(BetaAvg[[1]],BetaAvg[[2]])
for(i in 3:609){
  m1 <- rbind(m1,BetaAvg[[i]])
}
write.csv(m1,file="betaAvg.csv")
m1
AMSP <- AA
BLL <- BB
AMSP <- AMSP[which(AMSP[,1]!=0),]
3600/20
for ( i in 1:length(unique(AMSP[,2]))){
  cat("\n",length(AMSP[,2][which(AMSP[,2]==unique(AMSP[,2])[i])]))
}
write.csv()
AMSP1 <- AMSP[which(AMSP[,2]!=max(AMSP[,2])),]
BLL1 <- BLL[which(BLL[,2]!=max(BLL[,2])),]
lfejl <- cbind(BLL1,rep(1:180,length(unique(BLL1[,2]))));lfejl <- as.data.frame(lfejl)
mspefejl <- cbind(AMSP1,rep(1:180,length(unique(AMSP1[,2]))));mspefejl <- as.data.frame(mspefejl)
colnames(lfejl) <- c("Fejl","Lambda","nr");colnames(mspefejl) <- colnames(lfejl)
length(lfejl$Lambda)/180
length(lambdatest)
lambdatest[609]
lambdatestt <- lambdatest[which(lambdatest<20.01003)]
lfejlgns=0;counter=0;mspefejlgns=0;
for (i in lambdatestt){
  counter=counter+1
lfejlgns[counter] <- mean(lfejl[which(lfejl$Lambda==i),1])
mspefejlgns[counter] <- mean(mspefejl[which(mspefejl$Lambda==i),1])
}
lfejlgns <- cbind(lfejlgns,lambdatestt)
mspefejlgns <- cbind(mspefejlgns,lambdatestt)
str(lfejl)
plot(-lfejlgns)
min(-lfejlgns[,1])

lfejlgns[which(-lfejlgns==min(-lfejlgns[,1])),]
mspefejlgns[which(mspefejlgns==min(-mspefejlgns[,1])),]

lfejlgns$lambda[which(-lfejlgns[,1]==1.033374)]
ggplot() + ylab("MSPE") + xlab("??")+
  #theme_bw() +
  #theme(panel.grid=element_blank()) +
  geom_line(size=0.22,alpha=0.35,data=mspefejl[which(mspefejl$Lambda<= 0.02),], aes(x=Lambda, y=Fejl, group=as.factor(nr),color=as.factor(nr))) + #geom_line(data = meanfejl,aes(x = Lambda,y = Fejl),size=1) +
  theme(legend.position = "none")+
  geom_line(data = as.data.frame(mspefejlgns)[which(mspefejl$Lambda<= 0.02),], aes(x=lambdatest, y=mspefejlgns))
#scale_colour_binned(palette="Greens")+
?ggplot
min(lfejl$Lambda)
bs=0;
for(i in 1:500){
bs[i] <- sum(abs(BetaAvg[[i]][,1]))
  }
plot(bs)
  beta0
  BetaAvg <- na.omit(BetaAvg)
  BetaAvg <- as.numeric(BetaAvg)
  write.csv(BetaAvg,file="betaAvg.csv")
  names(BetaAvg)=
  str(BetaAvg)
  write.csv(cbind(lfejl,mspefejl),file="Statisk_Krydsval20.csv")
#SJOVT PLOT BETAE KOEFFICIENTER MOD LAMBDA
m <- CreateMatrixes1(data,"2015-07-17","2016-05-29",0)
x <- m$DesignMatrix;Y<-m$KontingensTabel;r<-m$SamledeKampe;koef=cbind(c(0),c(0));tbeta = beta0;
for(i in 1:800){
  f <- BTFunktioner1(beta,theta,x,Y,r)
  n <- NR1(SBeta=tbeta,x,f,lambda=(lambdatest[i]-1));beta <- n$beta;theta<-n$theta;beta
  tbeta <- beta
  lengthbeta <- length(beta)
  length(beta)=length(beta0)
  names(beta) <- c(names(beta[1:(lengthbeta)]),names(beta0[which(!(names(beta0)%in%names(beta)))]))
  koef<-cbind(c(append(koef[,1],beta,after=length(koef[,1]))),c(append(koef[,2],rep(lambdatest[i]-1,10),after=length(koef[,1]))))
}
koeff <- koef
koef <- koeff
koefBB <- koef
koef <- koeff
koef[is.na(koef)]=0
koef <- koef[-1,]
colnames(koef)=c("val","lambda","Variable")
koef <- cbind(koef,c(rownames(koef)))
koef <- as.data.frame(koef)
koef$lambda <- as.numeric(koef$lambda)
koef$val <- as.numeric(koef$val)

str(koef)
?replace
koef$V3 <-replace(koef$V3,koef$V3=="GnsBoldBes","BB")
koef$V3 <-replace(koef$V3,koef$V3=="GnsFrispark","Fris")
koef$V3 <-replace(koef$V3,koef$V3=="GnsHjorne","Hjrne")
koef$V3 <-replace(koef$V3,koef$V3=="GnsMal","Mal")
koef$V3 <-replace(koef$V3,koef$V3=="GnsMalInd","MalI")
koef$V3 <-replace(koef$V3,koef$V3=="GnsOffside","Offs")
koef$V3 <-replace(koef$V3,koef$V3=="GnsSkud","Skud")
koef$V3 <-replace(koef$V3,koef$V3=="GnsSkudIndenfor","SkudI")
koef$V3 <-replace(koef$V3,koef$V3=="GnsTilskuer","Tils")
koef$V3 <-replace(koef$V3,koef$V3=="TeamRatings","Fifa")
#write.csv(koef,file="SKL1.csv")





?scale_x_continuous
ggplot(data = koef[which(koef$lambda<=55),],aes(x=log(lambda),y=val,col=Variable)) + ylab("Koefficienter") + xlab("log??")+

    #theme_bw() +
  #theme(panel.grid=element_blank()) +
  #theme(legend.position = "none")+
  scale_color_manual(values = c("coral4","dodgerblue4","antiquewhite4","chartreuse1","slateblue",
                                "burlywood","magenta1","blue1","orangered1","yellowgreen"))+
scale_x_continuous( sec.axis =~ exp(.))+
geom_line()+
geom_hline(aes(yintercept=0),colour = 'white',size=0.1)+
geom_dl(aes(label=Variable),method=list(dl.combine("first.points"),cex=0.8))
#scale_colour_binned(palette="Greens")+
