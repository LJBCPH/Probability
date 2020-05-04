require(matlib) #Til vektor/matrix regning
require(blockmatrix) #matrixopsætning
require(tidyr) #Til data transformation
require(MASS)
CreateMatrixes <- function(data,StartDate,EndDate,round,TR = FALSE) {
  #fikser datatypes
  data$H <- as.character(data$H)
  data$U <- as.character(data$U)
  data$dato <- as.Date(data$dato, format = "%m/%d/%Y")
  #Henter 1.5 sæson ud:
  #StartDate = "2015-07-17";EndDate = "2016-05-29";round = 2;
  data2 <- data[which((data$dato>=StartDate) & (data$dato <= EndDate) & (data$runde == round)),]
  data1 <- data[which((data$dato>=StartDate) & (data$dato <= EndDate) & (data$runde < round)),]
  #data1 <- data1[(data1$H %in% data2$H) | (data1$U %in% data2$U),];
  #data1 <- data1[data1$U %in% data2$U,];
  #Finder holdene der er i Superligaen i indeværende sæson
  dataUNan <- na.omit(data1)
  #sætter Y-kontingenstabellen op:
  #Samler strengen af hjemme mod ude
  data2$HU <- paste(data2$H,data2$U)
  UnikSammenligner <- unique(data2$HU)
  #Udregner antal hjemmesejre og udesejre i kombinationerne
  hjemmesejre <- aggregate(data2$Hsejr, by = list(HU=data2$HU),FUN=sum);hjemmesejre <- hjemmesejre[order(hjemmesejre$HU),]
  udesejre <- aggregate(data2$Usejr, by = list(HU=data2$HU),FUN=sum);udesejre <- udesejre[order(udesejre$HU),]
  #Data fix
  udesejre <- separate(data=udesejre,col=HU,into=c("H1","H2"),sep=" ")
  hjemmesejre <- separate(data=hjemmesejre,col=HU,into=c("H1","H2"),sep=" ")
  #bytter kolonner
  udesejre <- udesejre[(c("H2","H1","x"))];names(udesejre) <- c("H1","H2","x")
  #udesejre$HU <- paste(udesejre$H2,udesejre$H1);udesejre <- udesejre[,4:3];#udesejre <- udesejre[order(udesejre$HU),]
  #HUsejre <- c(hjemmesejre$x+udesejre$x)
  samlet <- rbind.data.frame(hjemmesejre,udesejre)
  #colnames(samlet)[1] <- "Hold"
  #UnikSammenligner <- separate(data = as.data.frame(UnikSammenligner),col = UnikSammenligner,into = c("H1","H2"),sep = " ")
  #UnikHold
  #samlet <- separate(data=samlet,col=Hold,into=c("H1","H2"),sep=" ")
  #tsamlet <- cbind.data.frame(samlet$H2,samlet$H1,rep(0,length(samlet[,1])))
  #names(tsamlet) <- c("H1","H2","HUsejre")
  #samlet <- rbind(samlet,tsamlet)
  Y <- xtabs(samlet$x~(H1+H2),samlet)
  Y <- as.data.frame.matrix(Y)
  #Danner designmatricen
  UnikHold <- cbind(unique(c(data1$H,data1$U)))
  UnikHold <- sort(UnikHold)
  streak <- c(rep(1,length(UnikHold)))
  #Danner ikke-tab-streak
  for (hold in 1:length(UnikHold)){
    StreakSum = 0
    for (kamp in 1:length(data1[,1])) {
      if (data1$H[kamp] == UnikHold[hold]) {
        if (data1$HM[kamp] > data1$UM[kamp]){
          StreakSum = StreakSum + 1
        } else {
          StreakSum = 0
        }
      } else if (data1$U[kamp] == UnikHold[hold]) {
        if (data1$HM[kamp] < data1$UM[kamp]){
          StreakSum = StreakSum + 1
        } else {
          StreakSum = 0
        }
      }
    }
    streak[hold] = StreakSum
  }

  Mal <- c(aggregate(data1$HM, by = list(H = data1$H),FUN = sum)[,2]+aggregate(data1$UM, by = list(U = data1$U),FUN = sum)[,2])
  MalInd <- c(aggregate(data1$UM, by = list(H = data1$H),FUN = sum)[,2]+aggregate(data1$HM, by = list(U = data1$U),FUN = sum)[,2])
  GnsTilskuer <- c(aggregate(data1$Tilskuere, by = list(H = data1$H),FUN = sum)[,2]+aggregate(data1$Tilskuere, by = list(U = data1$U),FUN = sum)[,2])/(round-1)
  GnsBoldBes <- c(aggregate(dataUNan$boldb_h, by = list(H = dataUNan$H),FUN = sum)[,2]+aggregate(dataUNan$boldb_u, by = list(U = dataUNan$U),FUN = sum)[,2])/(round-1)
  GnsSkud <- c(aggregate(dataUNan$skud_h, by = list(H = dataUNan$H),FUN = sum)[,2]+aggregate(dataUNan$skud_u, by = list(U = dataUNan$U),FUN = sum)[,2])/(round-1)
  GnsSkudIndenfor <- c(aggregate(dataUNan$skudindenfor_h, by = list(H = dataUNan$H),FUN = sum)[,2]+aggregate(dataUNan$skudindenfor_u, by = list(U = dataUNan$U),FUN = sum)[,2])/(round-1)
  GnsFrispark <- c(aggregate(dataUNan$frispark_h, by = list(H = dataUNan$H),FUN = sum)[,2]+aggregate(dataUNan$frispark_u, by = list(U = dataUNan$U),FUN = sum)[,2])/(round-1)
  GnsHjorne <- c(aggregate(dataUNan$hjorne_h, by = list(H = dataUNan$H),FUN = sum)[,2]+aggregate(dataUNan$hjorne_u, by = list(U = dataUNan$U),FUN = sum)[,2])/(round-1)
  GnsOffside <- c(aggregate(dataUNan$offside_h, by = list(H = dataUNan$H),FUN = sum)[,2]+aggregate(dataUNan$offside_u, by = list(U = dataUNan$U),FUN = sum)[,2])/(round-1)
  #TeamRatings <- c(67,63,66,67,66,72,69,66,64,63,66,64,65,62,64,64)
  TeamRatings <- c(66,65,67,65,71,69,64,64,64,66,65,63)
  TeamRatings <- cbind(c(UnikHold),TeamRatings)
  TeamRatings <- TeamRatings[TeamRatings[,1] %in% UnikHold,];
  FifaRating <- as.numeric(TeamRatings[,2])
  HjemmeBane <- c(rep(0,length(UnikHold)))
  for (hold in 1:length(UnikHold)) {
    for (kamp in 1:length(data2$H)) {
      if (data2$H[kamp] == UnikHold[hold]) {
        HjemmeBane[hold] = 1;
      }
    }
  }

  x <- as.data.frame.matrix(rbind(HjemmeBane,streak,FifaRating,GnsHjorne,GnsOffside,Mal,MalInd,GnsTilskuer,GnsBoldBes,GnsSkud,GnsSkudIndenfor,GnsFrispark))
  #x <- as.data.frame.matrix(prop.table(as.matrix(x),1))
  names(x) <- names(Y)
  #x <- as.matrix(rbind(GnsMal,GnsBoldBes,GnsSkud))
  #Danner Antal-kampe-vektoren (r)
  #rsamlet <- cbind.data.frame(data2$U,data2$H,rep(1,length(data2[,1])),rep(0,length(data2[,1])),rep(0,length(data2[,1])))
  #names(rsamlet) <- c("data2$H","data2$U","data2$Hsejr","data2$Usejr","data2$Uafgjort")
  #rsamlet2 <- rbind(cbind.data.frame(data2$H,data2$U,data2$Hsejr,data2$Usejr,data2$Uafgjort),rsamlet)
  #names(rsamlet2) <- c("H","U","Hsejr","Usejr","Uafgjort")
  rsamlet <- cbind.data.frame(samlet$H1,samlet$H2,rep(1,length(samlet$H1)))
  rsamlet2 <- rsamlet
  names(rsamlet2) <- c("H","U","Total")
  levels(rsamlet2$U) <- sort(levels(rsamlet2$U))
  levels(rsamlet2$H) <- sort(levels(rsamlet2$U))
  #r <- xtabs((rsamlet2$Hsejr+rsamlet2$Usejr+rsamlet2$Uafgjort)~(H+U),rsamlet2)
  r <- xtabs((rsamlet2$Total)~(H+U),rsamlet2)
  r <- as.data.frame.matrix(r)
  #r <- r+t(r)
  Matrixes <- list("DesignMatrix" = x,"KontingensTabel" = Y,"SamledeKampe" = r)
  return(Matrixes)
}

BTFunktioner <- function(beta,theta,x,Y,r) {
  logl <- function(beta,theta,x){
    sum=0;
    for (i in 1:(dim(x)[2]-1)){
      for (j in (i+1):(dim(x)[2])){
        sum = sum + Y[i,j]*(t(x[,i])%*%beta-log(exp(t(x[,i])%*%beta)+theta*exp(t(x[,j])%*%beta)))+
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

NR <- function(x,Beta,Theta,Sbeta,Stheta,eps = 0.00000001,MaxIte = 300) {
  if(missing(Sbeta)) {
    itebeta <- c(rep(0,dim(x)[1]))
  } else {
    itebeta <- Sbeta
  }
  if(missing(Stheta)) {
    itetheta <- 1.1
  } else {
    itetheta <- Stheta
  }
  ite = as.matrix(c(itebeta,itetheta))
  #ite = as.matrix(c(rep(0,dim(x)[1]),1.55));
  counter=0;val=1;StepHalv = 1/2;
  while(abs(val)>eps){
    if(missing(Beta)){
      beta = c(ite[1:(dim(x)[1])]);
    } else {
      beta = Beta
    }
    if(missing(Theta)){
      theta=ite[dim(x)[1]+1];
    } else {
      theta = Theta
    }
    #a12 = as.matrix(f$t2);
    #grad = rbind(a12,f$t3);
    #A = f$t6;B = as.matrix(f$t5);C = t(as.matrix(f$t5));D = f$t4;
    #hess = cbind2(A,B);hess = rbind(hess,c(C,D));inf=-hess;
    if(counter > 1 && (-D < 0 || -A+B%*%D^(-1)%*%C < 0)){
      cat("Ude af maengden, step tilbage",counter)
      for (runde in 2:33){
        m <- CreateMatrixes(data,"2015-07-17","2016-05-29",runde,TRUE)
        x <- m$DesignMatrix;Y <- m$KontingensTabel; r <- m$SamledeKampe;
        f <- BTFunktioner(x=x,Y=Y,r=r)
        if (runde == 2) {
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
      a12 = as.matrix(f$t2);
      grad = rbind(a12,f$t3);
      A = f$t6;B = as.matrix(f$t5);C = t(as.matrix(f$t5));D = f$t4;
      hess = cbind2(A,B);hess = rbind(hess,c(C,D));inf=-hess;
      StepHalv = -1/4;
      temp = ite;
      ite = ite + inv(inf)%*%grad*StepHalv;
      val = sum(temp-ite);
      counter = counter +1;
      cat(counter)
    } else {
      for (runde in 3:33){
        m <- CreateMatrixes(data,"2015-07-17","2016-05-29",runde,TRUE)
        x <- m$DesignMatrix;Y <- m$KontingensTabel; r <- m$SamledeKampe;
        f <- BTFunktioner(x=x,Y=Y,r=r)
        if (runde == 3) {
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
      a12 = as.matrix(f$t2);
      grad = rbind(a12,f$t3);
      A = f$t6;B = as.matrix(f$t5);C = t(as.matrix(f$t5));D = f$t4;
      hess = cbind2(A,B);hess = rbind(hess,c(C,D));inf=-hess;
      StepHalv = 1/2;
      temp = ite;
      ite = ite + inv(inf)%*%grad*StepHalv;
      val = sum(temp-ite);
      counter = counter +1;
      cat("\n",counter,"\n","Likelihood:",f$t1,"\n Beta",beta,"\n theta",theta)
    }
    if(counter > MaxIte){
      cat("\n",counter,"\n","Likelihood:",f$t1,"\n Beta",beta,"\n theta",theta)
      break
    }
  }
  cat("logl :",f$loglike(beta,theta,x),"\n","Iterations: ", counter)
  styrker <- exp(t(x)%*%beta);names(styrker) <- names(Y)
  KV <- inv(inf)
  U <- sqrt(diag(KV))
  Values = list("beta" = beta, "theta" = theta,"styrker" = styrker,"sd" = U)
  return(Values)
}
Sandsynligheder <- function(theta,x,styrker,i,j){
  VTU=0;
  VTU = c((styrker[i])/(styrker[i]+theta*styrker[j]),
          ((styrker[j])/(styrker[i]*theta+styrker[j])),
          ((styrker[i]*styrker[j]*(theta^2-1))/((styrker[i]+theta*styrker[j])*(styrker[i]*theta+styrker[j])))
  )
  return(VTU)
}
