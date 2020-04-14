rm(list=ls())
library(matlib)
setwd("C:/Users/lucas/Desktop/Odd")
#Henter og verificerer data
data <- read.table("kampe_r.csv",header=T,sep=",")
head(data)
str(data)
#fikser datatypes
data$H <- as.character(data$H)
data$U <- as.character(data$U)
attach(data)
#sætter Y-kontingenstabellen op:
#hjemmesejre
aggregate(data$Hsejr,by=list(H=data$H),FUN=sum)
#udesejre
aggregate(data$Usejr,by=list(U=data$U),FUN=sum)

#Danner differentialerne
#Funktion til at omdanne Power til a^b
Power = function(a,b)a^b
#placeholder
beta = c(1.6758,0.0269404)
theta = 1.59384
dx2x <- deriv(~ x^2, "x") ; 
x <- cbind(c(0.23,4),c(0.67,29),c(0.51,7))
Y <- cbind(c(0,13,12),c(3,0,3),c(5,13,0))
E <- exp(1)
#Opskriver log-likelihoodfunktion
i=1;j=1;sum=0;r=20;
logl <- function(beta,theta,x){
  sum=0;
  for (i in 1:(dim(x)[2]-1)){
    for (j in (i+1):(dim(x)[2])){
      sum = sum +Y[i,j]*(t(x[,i])%*%beta-log(exp(t(x[,i])%*%beta)+theta*exp(t(x[,j])%*%beta)))+
                 Y[j,i]*(t(x[,j])%*%beta-log(exp(t(x[,j])%*%beta)+theta*exp(t(x[,i])%*%beta)))+
                 (r-Y[i,j]-Y[j,i])*(log(theta^2-1)+t(x[,i])%*%beta+t(x[,j])%*%beta-log(exp(t(x[,i])%*%beta)+
                 theta*exp(t(x[,j])%*%beta))-log(exp(t(x[,j])%*%beta)+theta*exp(t(x[,i])%*%beta)))
    }
 }
return(sum)
}
logl(beta,theta,x)

d2 = function(beta,theta){Y[2,1]*(x[1,2] - (Power(E,x[1,2]*beta[1] + x[2,2]*beta[2])*x[1,2] + 
                                             Power(E,x[1,1]*beta[1] + x[2,1]*beta[2])*x[1,1]*theta)/
                                      (Power(E,x[1,2]*beta[1] + x[2,2]*beta[2]) + Power(E,x[1,1]*beta[1] + x[2,1]*beta[2])*theta)) + 
                                 Y[1,2]*(x[1,1] - (Power(E,x[1,1]*beta[1] + x[2,1]*beta[2])*x[1,1] + 
                                               Power(E,x[1,2]*beta[1] + x[2,2]*beta[2])*x[1,2]*theta)/
                                        (Power(E,x[1,1]*beta[1] + x[2,1]*beta[2]) + Power(E,x[1,2]*beta[1] + x[2,2]*beta[2])*theta)) + 
                                 (20 - Y[1,2] - Y[2,1])*(x[1,1] + x[1,2] - 
                                                     (Power(E,x[1,2]*beta[1] + x[2,2]*beta[2])*x[1,2] + 
                                                        Power(E,x[1,1]*beta[1] + x[2,1]*beta[2])*x[1,1]*theta)/
                                                     (Power(E,x[1,2]*beta[1] + x[2,2]*beta[2]) + Power(E,x[1,1]*beta[1] + x[2,1]*beta[2])*theta) - 
                                                     (Power(E,x[1,1]*beta[1] + x[2,1]*beta[2])*x[1,1] + 
                                                        Power(E,x[1,2]*beta[1] + x[2,2]*beta[2])*x[1,2]*theta)/
                                                     (Power(E,x[1,1]*beta[1] + x[2,1]*beta[2]) + Power(E,x[1,2]*beta[1] + x[2,2]*beta[2])*theta))
  }
d2(beta,theta)

d1 = function(beta,theta){sum1 = Y[2,1]*(x[2,1] - (Power(E,x[2,1]*beta[1] + x[2,2]*beta[2])*x[2,1] + Power(E,x[1,1]*beta[1] + x[1,2]*beta[2])*x[1,1]*theta)/
                                           (Power(E,x[2,1]*beta[1] + x[2,2]*beta[2]) + Power(E,x[1,1]*beta[1] + x[1,2]*beta[2])*theta)) + 
  Y[1,2]*(x[1,1] - (Power(E,x[1,1]*beta[1] + x[1,2]*beta[2])*x[1,1] + Power(E,x[2,1]*beta[1] + x[2,2]*beta[2])*x[2,1]*theta)/
            (Power(E,x[1,1]*beta[1] + x[1,2]*beta[2]) + Power(E,x[2,1]*beta[1] + x[2,2]*beta[2])*theta)) + 
  (20 - Y[1,2] - Y[2,1])*(x[1,1] + x[2,1] - (Power(E,x[2,1]*beta[1] + x[2,2]*beta[2])*x[2,1] + Power(E,x[1,1]*beta[1] + x[1,2]*beta[2])*x[1,1]*theta)/
                            (Power(E,x[2,1]*beta[1] + x[2,2]*beta[2]) + Power(E,x[1,1]*beta[1] + x[1,2]*beta[2])*theta) - 
                            (Power(E,x[1,1]*beta[1] + x[1,2]*beta[2])*x[1,1] + Power(E,x[2,1]*beta[1] + x[2,2]*beta[2])*x[2,1]*theta)/
                            (Power(E,x[1,1]*beta[1] + x[1,2]*beta[2]) + Power(E,x[2,1]*beta[1] + x[2,2]*beta[2])*theta)) 
return(sum1)  
}
d1(beta,theta)
