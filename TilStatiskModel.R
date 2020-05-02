require(matlib) #Til vektor/matrix regning
require(blockmatrix) #matrixops�tning
require(tidyr) #Til data transformation

CreateMatrixes1 <- function(data,StartDate,EndDate,round) {
  #fikser datatypes
  data$H <- as.character(data$H)
  data$U <- as.character(data$U)
  data$dato <- as.Date(data$dato, format = "%m/%d/%Y")
  #Henter 1.5 s�son ud:
  data1 <- data[which((data$dato>=StartDate) & (data$dato <= EndDate)),]
  StartDate <- "2015-07-17";EndDate <- "2016-05-29"
  #Finder holdene der er i Superligaen i indev�rende s�son
  dataUNan <- na.omit(data1)
  #detach(data1)
  #s�tter Y-kontingenstabellen op:
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
x
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
  TeamRatings <- c(65,65,66,65,71,69,63,63,64,66,66,63)
  x <- as.data.frame.matrix(rbind(TeamRatings,GnsHjorne,GnsOffside,GnsMal,GnsMalInd,GnsTilskuer,GnsBoldBes,GnsSkud,GnsSkudIndenfor,GnsFrispark))
  names(x) <- names(Y)
  #x <- as.matrix(rbind(GnsMal,GnsBoldBes,GnsSkud))
  #Danner Antal-kampe-vektoren (r)
  r <- xtabs(data1$Hsejr+data1$Usejr+data1$Uafgjort~H+U,data1)
  r <- as.data.frame.matrix(r)
  r <- r+t(r)
  Matrixes <- list("DesignMatrix" = x,"KontingensTabel" = Y,"SamledeKampe" = r)
  return(Matrixes)
}

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

NR1 <- function(x,f,Beta) {
  ite = as.matrix(c(rep(0.1,dim(x)[1]),1.1));counter=0;val=1;
  while(abs(val)>0.0000001){
    if(ismissing(Beta)){
    beta = c(ite[1:(dim(x)[1])]);
    } else {
    beta = Beta
    }
    theta=ite[dim(x)[1]+1];
    a12 = as.matrix(f$dlbeta(beta,theta,x));
    grad = rbind(a12,f$dltheta(beta,theta,x));
    A = f$dl2xbeta(beta,theta,x);B = as.matrix(f$dlbetatheta(beta,theta,x));C = t(as.matrix(f$dlbetatheta(beta,theta,x)));D = f$dl2xtheta(beta,theta,x);
    inf = cbind(A,B);inf = rbind(inf,c(C,D));inf=-inf;
    temp = ite;
    ite = ite + Inverse(inf)%*%grad*(1/2);
    val = sum(temp-ite);
    cat(f$loglike(beta,theta,x),"   ",counter,"\n",beta,"\n",theta,"\n")
    counter = counter +1;
  }
  cat("logl :",f$loglike(beta,theta,x))
  styrker <- exp(t(x)%*%beta)
  KV <- inv(inf)
  U <- sqrt(diag(KV))
  Values = list("beta" = beta, "theta" = theta,"Styrker" = styrker,"sd" = U)
  return(Values)
}

Sandsynligheder1 <- function(beta,theta,x,i,j){
  VTU=0;
  VTU = c((styrker[i])/(styrker[i]+theta*styrker[j]),
          ((styrker[j])/(styrker[i]*theta+styrker[j])),
          ((styrker[i]*styrker[j]*(theta^2-1))/((styrker[i]+theta*styrker[j])*(styrker[i]*theta+styrker[j])))
  )
  return(VTU)
}

m <- CreateMatrixes1(data,"2015-07-17","2016-05-29",0)
x <- m$DesignMatrix;Y<-m$KontingensTabel;r<-m$SamledeKampe

f <- BTFunktioner1(beta,theta,x,Y,r)
n <- NR1(x,f)
statstyrker <- n$Styrker/min(n$Styrker)
b0 <- c(rep(0,length(n$beta)))
f$loglike(b0,n$theta,x)
round(n$beta,6)
round(n$theta,6)
round(n$sd,6)
