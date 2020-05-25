#git add Probabilities.R #git commit -m "Navn på ændring" #git push #git pull
rm(list=ls())
library(matlib) #Til vektor/matrix regning
library(MASS) # Til invers af numerisk problematiske matricer
library(blockmatrix) #matrixopsætning
library(tidyr) #Til data transformation
library(xtable) #Til Latex table
library(BTSoccer)
library(ggplot2) #plots
library(psych) #pairs.panel
library(SSLASSO)
library(installr)

setwd("C:/Users/Victo/Desktop/bachelor/kode")
setwd("C:/Users/lucas/Desktop/Odd")
#Henter og verificerer data
data <- read.table("Kampe_r1.csv",header=T,sep=",")
data$H <- as.character(data$H)
data$U <- as.character(data$U)
data$dato <- as.Date(data$dato, format = "%m/%d/%Y")
m <- CreateMatrixes(data,"2015-07-17","2016-05-29",33,Lambda = 0,alpha=33,model="Dyn")
x <- m$DesignMatrix;Y <- m$KontingensTabel;r <- m$SamledeKampe;
n <- NR(StartDate = "2015-07-17",EndDate = "2016-05-29",x=x,lambda=17,Sbeta=beta,RoundList = c(3:33),MaxIte = 200,eps = 0.0000001,LambLimit = 10^(-4),c = 0.000000000000000001,data=data,alpha=3,model="Dyn");
n <- NR1(StartDate = "2015-07-17",EndDate = "2016-05-29",x=x,lambda=0,RoundList = c(3:33),MaxIte = 200,eps = 0.0000001,LambLimit = 10^(-4),c = 0.000000000000000001,data=data);

test <- data[which(data$dato>="2015-07-17" & data$dato<="2016-05-19"),]
save(data, file = "myData.RData")
xtable(t(x))

KrydsVal <- cbind(c(4,8),c(9,13),c(14,18),c(19,23),c(24,28),c(29,33))
#2014 test
#beta <- c(0.13942804,-0.14948359,0.29691068,0.14819015,-0.03749685,0.17004741,-0.06201752);theta=1.64
names(beta) <- c("HjemmeBane","streak","FifaRating","GnsHjorne","GnsOffside","Mal","GnsBoldBes")
Odds <- read.table("Odds_201415.csv",header=T,sep=",")
str(Odds)
Odds$dato <- as.Date(Odds$dato, format = "%m/%d/%Y")
colnames(Odds)[1] <- "runde"
max(Odds$dato)

for ( i in 3:33){
  m <- CreateMatrixes(data,"2015-07-17","2016-05-29",33,Lambda = 0,Variabler=beta,alpha=3)
  xtable(as.data.frame(m$DesignMatrix))
  Sstreak <- m$DesignMatrix[2,]
}
x <- m$DesignMatrix;Y <- m$KontingensTabel;r <- m$SamledeKampe;
x <- x[which(rownames(x)%in%names(beta)),]
BetaDynML <- n$beta;thetaML <- n$theta
beta <- n$beta;theta <- n$theta
n$styrkgemt

n$sd
nbeta <- round(beta,3)
nsd <- round(n$sd,3)
n$theta
nbeta
nsd
round(konf,3)
konf = cbind(rbind(nbeta,theta)-nsd*qnorm(0.975),rbind(nbeta,theta)+nsd*qnorm(0.975))

names(nsd) <- c(names(nbeta),"theta")

beta <- c(0.224,-0.38,0.061,-0.005)
names(beta) <- c("Mal","MalInd","GnsTilskuer","GnsFrispark")
m <- CreateMatrixes1(Odds,"2013-07-17","2016-05-29",0)
xs <- m$DesignMatrix
xs <- xs[which(rownames(xs)%in%names(beta)),]
sty <- exp(t(xs-xs[,8])%*%beta)

Styrk <- exp(t(x)%*%beta)*0;cdone = 0;
UnikHold <- colnames(x);TotalWinLose = 0;AllOddsReturn=0;TeamMatchUp=0;
data <- Odds;i = 3;Runde = 2;theta <- n$theta;beta <-n$beta
#beta <- BetaDynUL; theta <- 1.663
#beta <- c(0.138,-0.087,0.359,0.035,-0.015,0.017,0.083,0.091,-0.190);theta <- 1.643;
#names(beta) <- c("HjemmeBane","streak","FifaRating","GnsHjorne","GnsOffside","Mal","MalInd","GnsTilskuer","GnsFrispark")
#beta <- BetaDynML;theta<-thetaML
beta <- c(-0.316,-0.437,0.126,0.259,-0.683,0.453,0.210,0.160,-0.395,-0.214)
names(beta) <- c("FifaRating","GnsHjorne","GnsOffside","Mal","MalInd","GnsTilskuer","GnsBoldBes","GnsSkud","GnsSkudIndenfor","GnsFrispark")
AA <- as.matrix(rbind(c(0,0,0)));BB <- as.matrix(rbind(c(0,0,0)));NumbBeta = 0;a=0;b=0;SamSSH = 0;Runde = 2;
for (i in 3:33){
#  cat("\n",i,"\n")
  temp = 0;
  Runde = Runde +1
  m <- CreateMatrixes(Odds,"2013-07-17","2016-05-29",i,Lambda = 0,Variabler = beta,alpha=33)
  x <- m$DesignMatrix;Y <- m$KontingensTabel;r <- m$SamledeKampe;
  x <- x[which(rownames(x)%in%names(beta)),]
  styrker = exp(t(x)%*%beta)
  #Styrk = Styrk + exp(t(x)%*%beta)
  #cdone = cdone +1
  cat("\n F�R ",Runde,"\n")
  temp <- Odds[which(Odds$runde == Runde),]
  cat("\n EFTER",Runde,"\n")
  temp$HU <- paste(temp$H,temp$U)
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
  ssh <- KumSSH[KumSSH$H1H2 %in% temp$HU,]
  ssh <- as.data.frame(ssh)
  ssh$H <- as.numeric(as.character(ssh$H));ssh$U <- as.numeric(as.character(ssh$U));ssh$Uafgjort <- as.numeric(as.character(ssh$Uafgjort))
  ssh <- ssh[order(ssh$H1H2),]
  #for(i in 1:6){
  #ssh[i,1:3] <- rbind(c(0.456,0.322,0.222))
  #}
  temp <- temp[order(temp$HU),]
  TempSSH <- cbind(cbind(temp$OH,temp$OU,temp$OUAF,temp$Hsejr,temp$Usejr,temp$Uafgjort),cbind(ssh$H,ssh$U,ssh$Uafgjort))
  SamSSH <- rbind(SamSSH,TempSSH)
  #for (j in 1:6){
  #  ssh[j,1] = 1;
  #  ssh[j,2] = 1;
  #  ssh[j,3] = 1;
  #}
  #Laver matrice med 1 og -1 ift. hvad udfaldet blev(1 for den som udfaldet er, og -1 for resten da man taber der)
  OddsWinLose <- matrix(1:3,nrow=6,ncol=3)
  OddsReturn <- cbind(ssh$H*(temp$OH-1),ssh$U*(temp$OU-1),ssh$Uafgjort*(temp$OUAF-1))
  #find optimale odds
  #for (k in 1:6){
  #  if(temp$OH[k]<(1/ssh$H[k])){
  #    OddsReturn[k,1] = OddsReturn[k,1]
  #  } else {
  #    OddsReturn[k,1] = 0
  #    ssh[k,1] = 0
  #  }
  #  if(temp$OU[k]<(1/ssh$U[k])){
  #    OddsReturn[k,2] = OddsReturn[k,2]
  #  } else {
  #    OddsReturn[k,2] = 0
  #    ssh[k,2] = 0
  #  }
  #  if(temp$OUAF[k]<(1/ssh$Uafgjort[k])){
  #    OddsReturn[k,3] = OddsReturn[k,3]
  #  } else {
  #    OddsReturn[k,3] = 0
  #    ssh[k,3] = 0
  #  }
  #}
  for (k in 1:6){
    OddsWinLose[k,1] = if(temp$Hsejr[k] == 1){OddsReturn[k,1]}else{-ssh[k,1]}
    OddsWinLose[k,2] = if(temp$Usejr[k] == 1){OddsReturn[k,2]}else{-ssh[k,2]}
    OddsWinLose[k,3] = if(temp$Uafgjort[k] == 1){OddsReturn[k,3]}else{-ssh[k,3]}
  }
  #oddsreturn
  #OddsReturn <- cbind(ssh$H*temp$OH,ssh$U*temp$OU,ssh$Uafgjort*temp$OUAF)
  #sum(OddsReturn*OddsWinLose)
  AllOddsReturn <- rbind(AllOddsReturn,OddsWinLose)
  TeamMatchUp <- rbind(TeamMatchUp,cbind(temp$H,temp$U))
  TotalWinLose <- rbind(TotalWinLose,sum(OddsWinLose))
  #rowSums(OddsWinLose)
  A <- c(rowSums(as.matrix((cbind(temp$Hsejr,temp$Usejr,temp$Uafgjort)-cbind(ssh$H,ssh$U,ssh$Uafgjort))^2))) #(Y-Yhat)^2
  a <- c(append(a,A,after= length(a)))
  B <- c(rowSums((cbind(temp$Hsejr*log(ssh$H),temp$Usejr*log(ssh$U),temp$Uafgjort*log(ssh$Uafgjort))))) #log(Yhat)
  b <- c(append(b,B,after = length(b)))
}
mean(a[-1])#0.6178785, Dyn = 0.5895847
mean(-b[-1])#-1.03003, Dyn = 0.9926718
sum(TotalWinLose)/186
length(TotalWinLose)
AllOdds <- cbind(AllOddsReturn,TeamMatchUp)[-1,]
write.csv(SamSSH,"OddsReturnStat1.csv")
Styrk/min(Styrk)
sum(TotalWinLose)
ToTable <- AllOdds[,1:5]
rownames(ToTable) <- c()
rownames(ToTable)
xtable(ToTable[,1:5],display.)
str(KumSSH)

t1 <- (aggregate(as.numeric(KumSSH$H),by = list(H1 = KumSSH$H1),FUN = sum)[2]*3
       +aggregate(as.numeric(KumSSH$U),by = list(H2 = KumSSH$H2),FUN = sum)[2]*3
       +aggregate(as.numeric(KumSSH$Uafgjort),by = list(H1 = KumSSH$H1),FUN = sum)[2]*2)*(1+(198-length(KumSSH$H))/198)
rownames(t1) <- colnames(x)

sum(t1)

?xtable
#########FEJL FOR BETATING MED LASSO
Sbeta <- c(rep(0,12))
#names(Sbeta) <- rownames(x);
#YS = 0
#for(i in 3:33){
#m <- CreateMatrixes(data,"2015-07-17","2016-05-29",round = i, TR = FALSE, Lambda = 0, alpha = 33)
#YS = YS + m$SamledeKampe
#Y2 = Y2 + m$SamledeKampe
#cat(sum(m$SamledeKampe),"\n")
#}
#sum(YS)
#Y2 = 0
#m$SamledeKampe
x <- m$DesignMatrix;Y <- m$KontingensTabel;r <- m$SamledeKampe;
LogVec <- matrix(1:2,nrow=30,ncol=2);c=0;
for(alpha in 3:33){
  c = c+1
  n <- NR(x=x,Stheta = 1.57,Sbeta = beta,lambda=3,RoundList = c(3:33),MaxIte = 200,eps = 0.0000001,LambLimit = 10^(-4),c = 0.000000000000000001,data=data,alpha=3);
  LogVec[c,1] = n$loglike
  LogVec[c,2] = alpha
}
n$sd
n$styrkgemt/min(n$styrkgemt)
n$styrker/min(n$styrker)

n <- NR(x=x,Stheta = 1.57,lambda=0,RoundList = c(3:33),MaxIte = 200,eps = 0.0000001,LambLimit = 10^(-4),c = 0.000000000000000001,data=data,alpha=2);
beta <- n$beta;theta <- n$theta
plot(LogVec[,1]~LogVec[,2],type='line')
beta <- n$beta;theta <- n$theta;

n$sd
n$beta
Sbeta <- beta[-10]
Sbeta <- c(0.156,-0.195,0.453,0.305,-0.079,0.259,0.106,-0.147,-0.156,-0.011,-0.007,-0.052)
#betaDYN <- beta
names(Sbeta) <- rownames(x)
rownames(x)
x <- x[which(rownames(x)%in%names(beta0)),]
beta0 <- beta
x[which(rownames(x)%in%names(beta)),]
LambdaValues <- c(0,0.5,0.75,1,1.5,2,2.5,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,25,35)
alpha = 3;beta0 <- beta
koef<-cbind(c(append(koef[,1],beta,after=length(koef[,1]))),c(append(koef[,2],rep(0,10),after=length(koef[,1]))))
koef = cbind(0,0);beta0 <- betat;c=0
for(i in LambdaValues){
  c=c+1
  n <- NR(x=x,Stheta = theta,Sbeta = beta0,lambda=LambdaValues[c],RoundList = c(3:33),MaxIte = 50,eps = 0.0000001,LambLimit = 10^(-4),c = 0.000000000000000001,data=data,alpha=3);
  beta0 <- n$beta
  x <- x[which(rownames(x)%in%names(beta0)),]
  lengthbeta <- length(beta0)
  length(beta0)=length(betat)
  names(beta0) <- c(names(beta0[1:(lengthbeta)]),names(betat[which(!(names(betat)%in%names(beta0)))]))
  koef<-cbind(c(append(koef[,1],beta0,after=length(koef[,1]))),c(append(koef[,2],rep(LambdaValues[c],12),after=length(koef[,1]))))
}
koeff <- as.data.frame(koef)
rownames(koeff)
ggplot(koeff,aes(x=V2, y = V1,group = V1))+geom_line()

LambdaValues <- c(21,22,23,24,25,26,27,28,29,30,31,32,33,34,35)
LambValues <- c(7,8,9,11,13,16,18,19)
LambdaValues <- c(unique(m4$Lambda))
UnikHold <- c(names(Y))
beta0 <- beta
AA <- as.matrix(rbind(c(0,0,0)));BB <- as.matrix(rbind(c(0,0,0)));NumbBeta = 0;BetaAvg <- vector("list",length(LambdaValues))
for(Lambda in LambdaValues){
  NumbBeta = NumbBeta + 1
  for(Kryds in 1:6){
    SS <- c(KrydsVal[1,Kryds],KrydsVal[2,Kryds])
    if(Kryds < 6){
    RoundList = c(3:(SS[1]-1),(SS[2]+1):33)
    } else {
    RoundList = c(3:28)
    }
    m <- CreateMatrixes(data,"2015-07-17","2016-05-29",SS[1]+1,TRUE,Lambda = Lambda,Variabler = beta0,alpha=alpha)
    x <- m$DesignMatrix;Y <- m$KontingensTabel; r <- m$SamledeKampe;f <- BTFunktioner(x=x,Y=Y,r=r);
    beta0 <- beta0[which(names(beta0)%in%rownames(x))]
    cat("\nher1\n",beta0,"\n")
    n <- NR(x=x,Stheta = 1.57,Sbeta = beta0,lambda=Lambda,RoundList = RoundList,MaxIte = 15,eps = 0.0000001,LambLimit = 10^(-4),c = 0.000000000000000001,alpha=alpha);
    beta <- n$beta;theta <- n$theta;
    TData <- data[which((data$runde >= SS[1] & data$runde <= SS[2])&(data$dato>="2015-07-17") & (data$dato <= "2016-05-29")),]
  #Sandsynligheder ift. beta osv
  a = 0;b=0;aa=0;bb=0;
  aa <- as.matrix(rbind(c(0,0,0)));bb <- as.matrix(rbind(c(0,0,0)));cc <- as.matrix(rbind(c(0,0,0,"H","U")));
  beta0 = n$beta;theta = n$theta;BetaAvgerage = 0;
  cat("\nher2\n",beta0,"\n")
  for (runde in SS[1]:SS[2]){
    cat("\nher3\n",beta0,"\n")
    m <- CreateMatrixes(data,"2015-07-17","2016-05-29",runde,TRUE,Lambda = Lambda,Variabler = beta0,alpha=alpha)
    x <- m$DesignMatrix;Y <- m$KontingensTabel; r <- m$SamledeKampe;f <- BTFunktioner(x=x,Y=Y,r=r);
    data1 <- data[which((data$dato>="2015-07-17") & (data$dato<="2016-05-29") & (data$runde == runde)),]
    data1$HU <- paste(data1$H,data1$U)
    x <- x[which(rownames(x) %in% names(beta0)),]
    styrker <- exp(t(x)%*%beta0)
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
      B <- c(rowSums((cbind(data1$Hsejr*log(ssh$H),data1$Usejr*log(ssh$U),data1$Uafgjort*log(ssh$Uafgjort))))) #log(Yhat)
      b <- c(append(b,B,after = length(b)))
#      length(beta0) <- length(BetaAvgerage)
      BetaAvgerage <- BetaAvgerage+beta0
    }
  AA <- cbind(c(append(AA[,1],a,after=length(AA[,1]))),c(append(AA[,2],rep(Lambda,length(a)),after=length(AA[,1]))))
  BB <- cbind(c(append(BB[,1],b,after=length(BB[,1]))),c(append(BB[,2],rep(Lambda,length(b)),after=length(BB[,1]))))
  #BetaAvg <- cbind(c(append(BetaAvg[,1],BetaAvg/6,after=length(BetaAvg[,1]))),c(append(BetaAvg[,2],rep(Lambda,length(beta)),after=length(BetaAvg[,1]))))
  BetaAvg[[NumbBeta]] <- cbind(c(BetaAvgerage/6),c(rep(Lambda,length(BetaAvgerage))))
  cat("\n\n\n",Kryds,"\n\n\n")
  }
}
AAA <- AA
unique(ADone[,2])
ADone <- rbind(AA,AAA1[which(!(AAA1[,2]%in%AA[,2])),])
BDone <- rbind(BB,BBB1[which(!(BBB1[,2]%in%BB[,2])),])

AA[which(!(AA[,2]%in%AAA[,2])),]
AAA1 <- rbind(AAA,AA[which(!(AA[,2]%in%AAA[,2])),])
BBB <- BB
BBB1 <- rbind(BBB,BB[which(!(BB[,2]%in%BBB[,2])),])

AA <- AA[which(AA[,2]<=20),]
BB <- BB[which(BB[,2]<=20),]
unique(AA[,2])
AAA <- AA[which(AA[,1]!=0),]
BBB <- BB[which(BB[,1]!=0),]

colnames(AAA) <- c("Y-Yhat^2","lambda")
colnames(BBB) <- c("log(Yhat)*Y","lambda")
m1 <- merge(BetaAvg[1],BetaAvg[2],by="ROWNAMES",all=T)
m1 <- rbind(BetaAvg[[1]],BetaAvg[[2]])
for(i in 4:33){
  m1 <- rbind(m1,BetaAvg[[i]])
}
SamletFejl <- cbind(AAA,BBB)
for (i in 1:(length(BetaAvg)-3)){
  print(sum(SamletFejl[,1][which(SamletFejl[,2]==LambdaValues[i])]))
}
LambdaValues[11]
AABB <- cbind(ADone,BDone)
write.csv(m1, file = "Lasso_Fejl_beta_SAMLET.csv")

m1 <- merge(BetaAvg[1],BetaAvg[2],by="ROWNAMES",all=T)
for(i in 3:21){
  m1 <- merge(m1,BetaList[i],by="ROWNAMES",all=T)
  #m1 <- m1[(length(m1[,1])-11):length(m1[,1]),]
  m1 <- m1[which(m1$ROWNAMES!=""),]
}
#startbeta = beta
n <- length(beta0)
#fixer length
length(beta305) <- n
#length(Beta0) <- n
#names(beta305) <- c("FifaRating")
beta05
Beta05 <- as.data.frame(beta05,row.names = names(beta05))
BetaList <- list(Beta0,Beta05,Beta075,Beta1,Beta0125,Beta015,Beta2,Beta25,Beta3,Beta35,Beta4,Beta45,Beta5,Beta6,
                 Beta10,Beta125,Beta15,Beta175,Beta20,Beta205,Beta305)
#b1
#b0125 <- sum(abs(beta));beta0125 <- beta #3  -183.8938, 9 beta
#b015 <- sum(abs(beta));beta015 <- beta #3 -184.1924, 8 beta
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
#beta305 <- c(0.03629097) #-198.5425, 1 beta

#library(plyr)
#join_all(BetaList,type="full")

for(i in 1:length(BetaList)){
  colnames(BetaList[[i]]) <- paste0( names(BetaList)[i], "_", colnames(BetaList[[i]]) )
  BetaList[[i]]$ROWNAMES  <- rownames(BetaList[[i]])
}
length(BetaList)
m1 <- merge(BetaList[1],BetaList[2],by="ROWNAMES",all=T)

merge(m1,BetaList[3],by="ROWNAMES",all=T)
for(i in 3:21){
  m1 <- merge(m1,BetaList[i],by="ROWNAMES",all=T)
  #m1 <- m1[(length(m1[,1])-11):length(m1[,1]),]
  m1 <- m1[which(m1$ROWNAMES!=""),]
}
m2 <- m1[-18]
m2 <- m2[-10]
m2[is.na(m2)] = 0
m3 <- as.data.frame(t(m2))
colnames(m3) = m2$ROWNAMES
m3 <- m3[-1,]
m3 <- as.data.frame(m3)
LambdaValues <- c(0,0.5,0.75,1,1.25,1.5,2,2.5,3,3.5,4,4.5,5,6,10,12.5,15,17.5,20,25,35)
LambdaValues <- LambdaValues[-18]
LambdaValues <- LambdaValues[-10]
#rownames(m3) <- c(LambdaValues)
m3$Lambda <- c(LambdaValues)
m4 <- read.table(file.choose(),header=T,sep=",")
colnames(m5) <- c("value","Lambda","variable")
names(m4)
m4 <- melt(m3,id.vars="λ")
m5$value <- as.numeric(as.character(m5$value))
m5$Lambda <- as.numeric(as.character(m5$Lambda))
m5 <- as.data.frame(m5)
str(m5)
m4
m5 <- cbind(m1,c(rownames(m1)))
ggplot(m5, aes(Lambda,value, col=variable)) +
  geom_line() + ylab("Koefficienter") +xlab("λ") +  #expand_limits(x = c(LambdaValues))
  geom_hline(aes(yintercept= 0), colour= 'white',size=0.1) +
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35,47))+
  scale_color_manual(values = c("coral4","dodgerblue4","antiquewhite4","deeppink1","chartreuse1","slateblue",
                                "burlywood","magenta1","blue1","orangered1","turquoise3","yellowgreen")) +
#scale_fill_viridis_d() +
  geom_dl(aes(label = variable), method = list(dl.combine("first.points"), cex = 0.8))

  scale_fill_brewer(palette="Spectral",guide="none")
scale_linetype_manual(name = "Observerede %-Udfald", values = c(rep(2,12)),
                      guide = guide_legend(override.aes = list(color = c("red", "blue","green"))))

#write.csv(m4, file = "beta_Lasso.csv")

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
#b35 <- c(0.03629097) #-198.5425, 1 beta

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
beta <- c(0.138,-0.087,0.359,0.035,-0.015,0.017,0.083,0.091,-0.190);theta <- 1.643
names(beta) <- c("HjemmeBane","streak","FifaRating","GnsHjorne","GnsOffside","Mal","MalInd","GnsTilskuer","GnsFrispark")
aa <- as.matrix(rbind(c(0,0,0)));bb <- as.matrix(rbind(c(0,0,0)));cc <- as.matrix(rbind(c(0,0,0,"H","U")));alpha=3
data <- Odds;
for (runde in 3:33){
  m <- CreateMatrixes(Odds,"2013-07-17","2016-05-29",round=runde,TRUE,Lambda=0,Variabler = beta,alpha=3)
  x <- m$DesignMatrix;Y <- m$KontingensTabel; r <- m$SamledeKampe;f <- BTFunktioner(x=x,Y=Y,r=r);
 # x <- x[which(rownames(x)%in%names(beta)),]
    data1 <- data[which((data$dato>="2013-07-17") & (data$dato<="2016-05-29") & (data$runde == runde)),]
  data1$HU <- paste(data1$H,data1$U)
  styrker <- exp(t(x-x[,8])%*%beta)
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
sum(as.numeric(cc$V1))/186
sum(as.numeric(cc$V2))/186
sum(as.numeric(cc$V3))/186
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
cc[1,]
#Estimererede Pointtildeling
cc <- as.data.frame(cc[-1,])
sump <- aggregate(as.numeric(as.character(cc[,1])),by = list(UnikHold=cc[,4]),FUN = sum)[,2]*3+aggregate(as.numeric(as.character(cc[,2])),by = list(UnikHold=cc[,5]),FUN = sum)[,2]*3+
aggregate(as.numeric(as.character(cc[,3])),by = list(UnikHold=cc[,4]),FUN = sum)[,2]+aggregate(as.numeric(as.character(cc[,3])),by = list(UnikHold=cc[,5]),FUN = sum)[,2]
sump = sump*(1+12/198)
names(sump) <- names(Y)
hist(y=sump,x=as.factor(names(sump)))
sum(sump)
n <- NR(x=x,Stheta = 1.64,Sbeta = beta,lambda=0,RoundList = c(3:33),MaxIte = 200,eps = 0.0000001,LambLimit = 10^(-4),c = 0.000000000000000001,data=data,alpha=3);
Sbeta <- n$beta;
StyrkerTid <- matrix(1:12,ncol=12,nrow=31);ST=cbind(0,0);Xgns=0;
for( i in 3:33){
m <- CreateMatrixes(data,"2015-07-17","2016-05-29",i,TRUE,alpha=3)
x <- m$DesignMatrix;Y <- m$KontingensTabel; r <- m$SamledeKampe;f <- BTFunktioner(x=x,Y=Y,r=r);
x <- x[which(rownames(x)%in%names(beta)),]
Xgns <- Xgns+(x-x[,8])
ST = rbind(ST,cbind(exp(t(x-x[,8])%*%beta),rep(i,12)))
#ST = rbind(ST,exp(t(x-x[,8])%*%beta))
if(i == 3){ST = ST[-1,]}
#ST = cbind(ST,c(rep(i,12)))
#StyrkerTid[i-2,1:12] = rbind(exp(t(x-x[,8])%*%beta),exp(t(x-x[,8])%*%beta))
}
#StyrkerTid <- as.data.frame(StyrkerTid)
#colnames(StyrkerTid) <- names(Y)
#t(StyrkerTid)
#melt(Styrker)
#ggplot(StyrkerTid, aes(x=1:31,y=StyrkerTid)) + geom_line()
ST <- cbind(ST,rownames(ST))
ST <- as.data.frame(ST)
names(ST) <- c("Styrke","Runde","Hold") 
ST$Styrke <- as.numeric(ST$Styrke)
ST$Runde <- as.numeric(ST$Runde)
cbind(aggregate(ST$Styrke,by = list( UnikHold = ST$Hold), FUN = mean),temp1)
xtable(x)
#aggregate(data2$Hsejr, by = list(HU=data2$HU),FUN=sum)
ggplot(ST, aes(Runde,Styrke, col=Hold)) +
  geom_line() + ylab("Koefficienter") +xlab("λ") +  #expand_limits(x = c(LambdaValues))
  geom_hline(aes(yintercept= 0), colour= 'white',size=0.1) +
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35,47))+
  scale_color_manual(values = c("coral4","dodgerblue4","antiquewhite4","deeppink1","chartreuse1","slateblue",
                                "burlywood","magenta1","blue1","orangered1","turquoise3","yellowgreen")) +
  #scale_fill_viridis_d() +
  geom_dl(aes(label = variable), method = list(dl.combine("first.points"), cex = 0.8))


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
