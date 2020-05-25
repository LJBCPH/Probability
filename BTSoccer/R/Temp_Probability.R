require(matlib) #Til vektor/matrix regning
require(blockmatrix) #matrixopsaetning
require(tidyr) #Til data transformation
require(MASS)
CreateMatrixes <- function(data,StartDate,EndDate,round,TR = FALSE,Lambda,Variabler,alpha,model) {
  #Opdeler data i at inkludere den valgte runde, samt alle runder foer runden.
  if(model == "Dyn"){
    data2 <- data[which((data$dato>=StartDate) & (data$dato <= EndDate) & (data$runde == round)),]
    data1 <- data[which((data$dato>=StartDate) & (data$dato <= EndDate) & (data$runde < round) & (data$runde>=(round-alpha))),]
  } else {
    data2 <- data[which((data$dato>=StartDate) & (data$dato <= EndDate)),]
    data1 <- data[which((data$dato>=StartDate) & (data$dato <= EndDate)),]
  }
  #Finder holdene der er i Superligaen i indevaerende saeson
  dataUNan <- na.omit(data1)
  #saetter Y-kontingenstabellen op:
  #Samler strengen af hjemme mod ude
  data2$HU <- paste(data2$H,data2$U)
  UnikSammenligner <- unique(data2$HU)
  #Udregner antal hjemmesejre og udesejre i kombinationerne
  hjemmesejre <- aggregate(data2$Hsejr, by = list(HU=data2$HU),FUN=sum);hjemmesejre <- hjemmesejre[order(hjemmesejre$HU),]
  udesejre <- aggregate(data2$Usejr, by = list(HU=data2$HU),FUN=sum);udesejre <- udesejre[order(udesejre$HU),]
  #Data fix til at faa ude og hjemme i rigtig raekkefoelge
  udesejre <- separate(data=udesejre,col=HU,into=c("H1","H2"),sep=" ")
  hjemmesejre <- separate(data=hjemmesejre,col=HU,into=c("H1","H2"),sep=" ")
  #bytter kolonner
  udesejre <- udesejre[(c("H2","H1","x"))];names(udesejre) <- c("H1","H2","x")
  samlet <- rbind.data.frame(hjemmesejre,udesejre)
  Y <- xtabs(samlet$x~(H1+H2),samlet)
  Y <- as.data.frame.matrix(Y)
  #Danner designmatricen
  UnikHold <- cbind(unique(c(data1$H,data1$U)))
  UnikHold <- sort(UnikHold)
  streak <- c(rep(1,length(UnikHold)))
  #Danner win-streak
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
  TeamRatings <- c(66,65,67,65,71,69,64,64,64,66,65,63) #FIFA RATINGS TIL SaeSON 2015-2016
  #TeamRatings <- c(66,67,64,69,66,61,63,62,63,65,61,57) #FIFA RATINGS TIL SaeSON 2014-2015
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
  if(model == "Dyn"){
  x <- t(standard(t(as.data.frame.matrix(rbind(HjemmeBane,streak,FifaRating,GnsHjorne,GnsOffside,Mal,MalInd,GnsTilskuer,GnsBoldBes,GnsSkud,GnsSkudIndenfor,GnsFrispark)))))
  #Fjerner de variabler fra designmatricen der er blevet fjernet v. lasso
  } else {
    x <- t(standard(t(as.data.frame.matrix(rbind(FifaRating,GnsHjorne,GnsOffside,Mal,MalInd,GnsTilskuer,GnsBoldBes,GnsSkud,GnsSkudIndenfor,GnsFrispark)))))
  }
  if(missing(Variabler)){}
  else{
  x <- x[which(rownames(x) %in% names(Variabler)),]
  }
  #standardiserer designmatricen
  x <- t(standard(t(x)))
  colnames(x) <- names(Y)
  #Danner r/R-matricen
  rsamlet <- cbind.data.frame(samlet$H1,samlet$H2,rep(1,length(samlet$H1)))
  rsamlet2 <- rsamlet
  names(rsamlet2) <- c("H","U","Total")
  levels(rsamlet2$U) <- sort(levels(rsamlet2$U))
  levels(rsamlet2$H) <- sort(levels(rsamlet2$U))
  r <- xtabs((rsamlet2$Total)~(H+U),rsamlet2)
  r <- as.data.frame.matrix(r)
  #Returnerer matricer
  Matrixes <- list("DesignMatrix" = x,"KontingensTabel" = Y,"SamledeKampe" = r)
  return(Matrixes)
}

#Rao-Kupper funktioner
BTFunktioner <- function(beta,theta,lambda=0,x,Y,r) {
  logl <- function(beta,theta,lambda=0,x){
    sum=0;
    for (i in 1:(dim(x)[2]-1)){
      for (j in (i+1):(dim(x)[2])){
        sum = sum + Y[i,j]*(t(x[,i])%*%beta-log(exp(t(x[,i])%*%beta)+theta*exp(t(x[,j])%*%beta)))+
          Y[j,i]*(t(x[,j])%*%beta-log(exp(t(x[,j])%*%beta)+theta*exp(t(x[,i])%*%beta)))+
          (r[i,j]-Y[i,j]-Y[j,i])*(log(theta^2-1)+t(x[,i])%*%beta+t(x[,j])%*%beta-log(exp(t(x[,i])%*%beta)+
           theta*exp(t(x[,j])%*%beta))-log(exp(t(x[,j])%*%beta)+theta*exp(t(x[,i])%*%beta)))
      }
    }
    return(sum-sum(lambda*abs(beta)))
  }

  db <- function(beta,theta,x,lambda=0){
    sum=0;
    for(i in 1:(dim(x)[2]-1)){
      for(j in (i+1):dim(x)[2]) {
        sum = sum + (c(r[i,j]-Y[i,j])*(-(theta*exp(t(x[,i])%*%beta))/(exp(t(x[,j])%*%beta)+theta*exp(t(x[,i])%*%beta)))
                     +c(r[i,j]-Y[j,i])*((theta*exp(t(x[,j])%*%beta))/(exp(t(x[,i])%*%beta)+theta*exp(t(x[,j])%*%beta)))
        )*(x[,i]-x[,j])
      }
    }
    return(sum-c(rep(lambda,length(sum))))
  }
  dbl <- function(beta,theta,x,l){
    sum=0;
    for(i in 1:(dim(x)[2]-1)){
      for(j in (i+1):dim(x)[2]) {
        sum = sum + (c(r[i,j]-Y[i,j])*(-(theta*exp(t(x[,i])%*%beta))/(exp(t(x[,j])%*%beta)+theta*exp(t(x[,i])%*%beta)))
                     +c(r[i,j]-Y[j,i])*((theta*exp(t(x[,j])%*%beta))/(exp(t(x[,i])%*%beta)+theta*exp(t(x[,j])%*%beta)))
        )*(x[,i]-x[,j])
      }
    }
    return(sum+c(rep(l,length(sum))))
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
  Funktions <- list("loglike" = logl,"dlbeta" = db,"dlassobeta"=dbl,"dltheta" = dtheta,"dl2xtheta" = dtheta2,"dl2xbeta"= db2,"dlbetatheta" = dbt)
  return(Funktions)
}

#Newton-Raphson
NR <- function(StartDate,EndDate,x,Beta,Theta,lambda=0,Sbeta,Stheta,RoundList,eps = 0.00000001,MaxIte = 300,LambLimit = 0.0001,c = 0.01,data,alpha,model) {
 #Tjekker om der er valgt en initialiseringsbeta
  styrkgemt <- c(rep(0,12));Xgns=0;
   if(missing(Sbeta)) {
    itebeta <- c(rep(0,dim(x)[1]))
  } else {
    itebeta <- Sbeta
  }
  #Tjekker om der er valgt en initialiseringstheta
    if(missing(Stheta)) {
    itetheta <- 1.1
  } else {
    itetheta <- Stheta
  }
  LassoRemoved <- 0
  ite = as.matrix(c(itebeta,itetheta))
  iteration=0;epsilon=1;StepHalv = 1/2;
  while(abs(epsilon)>eps){
#Tjekker om der er valgt fast beta
    if(missing(Beta)){
      beta = c(ite[-length(ite)]);
    } else {
      beta = Beta
          }
#Tjekker om der er valgt fast theta
    if(missing(Theta)){
      theta=ite[dim(x)[1]+1];
    } else {
      theta = Theta
    }
#Tjekker om vi er i den rigtige maengde
      if(iteration > 1 && (-D < 0 || det(-A+B%*%D^(-1)%*%C) < 0)){
      cat("\nUde af maengden, vaelg nye initialiseringsvaerdier")
      cat("\n logl :",f$loglike(beta,theta,lambda,x),"\n lambda :",lambda,"\n","Iterations: ", iteration,"\n")
      styrker <- exp(t(x)%*%beta);names(styrker) <- names(Y)
      KV <- inv(inf)
      U <- sqrt(diag(KV))
      names(beta) <- rownames(x)
      Values = list("loglike" = f$t1,"beta" = beta, "theta" = theta,"KV"=KV,"styrker" = styrker,"styrkgemt"=styrkgemt,"Xgns"=Xgns,"sd" = U)
      return(Values)
    } else {
      Xgns=0
      styrkgemt = 0
#Danner afledte til score og information
     for (runde in RoundList){
        m <- CreateMatrixes(data,StartDate,EndDate,runde,TRUE,Lambda = lambda,Variabler = Sbeta,alpha = alpha,model=model)
        x <- m$DesignMatrix;Y <- m$KontingensTabel; r <- m$SamledeKampe;
#Tjekker om der er blevet fjernet nogle parametre i lasso
        if(length(LassoRemoved)>1){
          for(Remove in 1:length(LassoRemoved[-1])){
            x <- x[-LassoRemoved[Remove+1],]
          }
        }
        Xgns = Xgns + x
        styrkgemt = styrkgemt+exp(t(x-x[,8])%*%beta)
        f <- BTFunktioner(x=x,Y=Y,r=r)
        if (runde == 3) {
          t1 <- f$loglike(beta,theta,0,x)
          t2 <- f$dlbeta(beta,theta,x,0)
          t3 <- f$dltheta(beta,theta,x)
          t4 <- f$dl2xtheta(beta,theta,x)
          t5 <- f$dlbetatheta(beta,theta,x)
          t6 <- f$dl2xbeta(beta,theta,x)
        } else {
          t1 <- t1 + f$loglike(beta,theta,0,x)
          t2 <- t2 + f$dlbeta(beta,theta,x,0)
          t3 <- t3 + f$dltheta(beta,theta,x)
          t4 <- t4 + f$dl2xtheta(beta,theta,x)
          t5 <- t5 + f$dlbetatheta(beta,theta,x)
          t6 <- t6 + f$dl2xbeta(beta,theta,x)
        }
      }
#Opstiller observationen og scorefunktionen til Newton skridt
      f$t1 <- t1-lambda*sum(sqrt(beta^2+c^2));f$t2 <- t2-lambda*(beta/sqrt(beta^2+c^2));f$t3 <- t3;f$t4 <- t4;f$t5 <- t5;f$t6 <- t6-lambda*(c^2)/((beta^2+c^2)^(3/2));
      a12 = as.matrix(f$t2);
      grad = rbind(a12,f$t3);
      A = f$t6;B = as.matrix(f$t5);C = t(as.matrix(f$t5));D = f$t4;
      hess = cbind2(A,B);hess = rbind(hess,c(C,D));inf=-hess;
#Finder skridtlaengden
      FoundStepLength = FALSE;StepHalv = 100; i = 0;testloglike = 0;
        while(i < 100 & FoundStepLength == FALSE & iteration >=1){
          testloglike <- 0
          i = i+1
          iteTest = ite + inv(inf)%*%grad*(1/i)
          betaa <- c(iteTest[-length(iteTest)]);
          thetaa=iteTest[dim(x)[1]+1];
            for (runde in RoundList){
            mm <- CreateMatrixes(data,StartDate,EndDate,runde,TRUE,Lambda = lambda,Variabler = Sbeta,alpha=alpha,model=model)
            xx <- mm$DesignMatrix;YY <- mm$KontingensTabel; rr <- mm$SamledeKampe;
            if(length(LassoRemoved)>1){
              for(Remove in 1:length(LassoRemoved[-1])){
                xx <- xx[-LassoRemoved[Remove+1],]
              }
            }
              g <- BTFunktioner(x=xx,Y=YY,r=rr)
              if (runde == 3) {
                testloglike <- g$loglike(betaa,thetaa,0,xx)
              } else {
                testloglike <- testloglike + g$loglike(betaa,thetaa,0,xx)
              }
          }
            if (f$t1 < testloglike -lambda*sum(sqrt(betaa^2+c^2))){
              StepHalv = i
              FoundStepLength = TRUE
            }
          }
      cat("\nStephalv",1/StepHalv,"\n")
      temp = ite;
#Udfoerer Newton-Raphson skridt
      ite = ite + inv(inf)%*%grad*(1/StepHalv);
      epsilon = sum(temp-ite);
#Tjekker om der skal fjernes parametre
      for (i in length(ite):1){
        if (length(ite) > 2 && iteration > 2 && lambda > 0 && ((abs(ite[i]) < LambLimit) || (sign(ite[i])!=sign(temp[i]) && abs(ite[i])<0.01))){
                   LassoRemoved <- cbind(LassoRemoved,i)
            # beta <- beta[-i]
            ite <- ite[-i]
            x <- x[-i,]
            cat("\nFjernet element nr. ",i,"\nlaengde ite",length(ite),"\n")
          }
      }
      loglike = f$t1
      iteration = iteration +1;
      cat("\nVal",abs(epsilon),"\niteration",iteration,"\n","Likelihood:",loglike,"\n Beta",beta,"\n theta",theta)
    }
    if(iteration > MaxIte){
      cat("\n",iteration,"\n","Likelihood:",f$t1,"\n Beta",beta,"\n theta",theta)
      break
    }
  }
  #Tjekker om de har skiftet fortegn ved konvergering
  Sbeta <- Sbeta[which(names(Sbeta)%in%names(beta))]
  for (i in length(ite):1){
    if (length(ite) > 2 && missing(Sbeta) == FALSE &&lambda > 0 && ((abs(ite[i]) < LambLimit) || (sign(ite[i])!=sign(temp[i]) && abs(ite[i])<0.01))|| (sign(ite[i]!=sign(Sbeta[i])))){
      LassoRemoved <- cbind(LassoRemoved,i)
      # beta <- beta[-i]
      ite <- ite[-i]
      x <- x[-i,]
      cat("\nFjernet element nr. ",i,"\nlaengde ite",length(ite),"\n")
    }
  }
#Newton-Raphson konvergering slut, returner vaerdier
  cat("\n logl :",f$t1,"\n lambda :",lambda,"\n","Iterations: ", iteration,"\n")
  styrker <- exp(t(x)%*%beta);names(styrker) <- names(Y)
  KV <- inv(inf)
  U <- sqrt(diag(KV))
  names(beta) <- rownames(x)
  Values = list("loglike" = f$t1,"beta" = beta, "theta" = theta,"KV"=KV,"styrker" = styrker,"styrkgemt"=styrkgemt,"Xgns"=Xgns,"sd" = U)
  return(Values)
}
#Danner sandsynligheder for hold i mod hold j, med given designmatrix
Sandsynligheder <- function(theta,x,styrker,i,j){
  VTU=0;
  VTU = c((styrker[i])/(styrker[i]+theta*styrker[j]),
          ((styrker[j])/(styrker[i]*theta+styrker[j])),
          ((styrker[i]*styrker[j]*(theta^2-1))/((styrker[i]+theta*styrker[j])*(styrker[i]*theta+styrker[j])))
  )
  return(VTU)
}


