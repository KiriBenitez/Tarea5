
### TAREA 5

##Pregunta 3

library(ggplot2)
library(dplyr)
library(gridExtra)

#Se genera la matriz de betas son de 8 columnas por que son 2 modelos y 4 tipo de poblaciones 
set.seed(150)
repeticiones=10000
betas=matrix(NA,nrow = repeticiones,ncol = 8)

beta0=2
beta1=2.5
beta2=1

su=1

n=c(50,100,500,1000)

#En esta parte lo que hace es que recorre cada tipo de poblacion, de acuerdo a las x sub i que existan

for (j in 1:length(n)) {
  x1=rnorm(n[j],20,1)
  x2=(0.8*x1)+rnorm(n[j],0,1)
  
  #En esta parte es un for oor lo cual aqui hace las 10.000 repeticiones de acuerdo a los modelos que se dan
  for (i in 1:repeticiones) {
    u=rnorm(n[j],0,su)
    Y0=beta0+beta1*x1+u
    Y1=beta0+beta1*x1+beta2*x2+u
    
    model0=lm(Y0~x1)
    betas[i,j]=model0$coef[2]
    
    model1=lm(Y1~x1+x2)
    betas[i,j+4]=model1$coef[2]
    # despues de las 10.000 vuele a saeguir con la otra poblacion de 100, 500 y 1000
  }
  
}

## Pregunta 3.1

betas=data.frame(betas)

apply(betas,2,mean)
apply(betas,2,var)

## Pregunta 3.2

g11=ggplot(betas) + geom_histogram(aes(betas[,5],y=..density..), col="black", bins = 30) +   #bins cantidad de barritas
  stat_function(fun = rnorm, args = list(mean=mean(betas[,5]), sd = sd(betas[,6])),
                geom = "line", col="red", size=1) +
  ylab("Densidad") + ggtitle("n=50") + xlab(expression(hat(beta)[1])) + 
  theme_bw()

g21 = ggplot(betas) +
  geom_histogram(aes(betas[,5],y=..density..), col="black", bins = 30) +   #bins cantidad de barritas
  stat_function(fun = rnorm, args = list(mean=mean(betas[,5]), sd = sd(betas[,7])),
                geom = "line", col="red", size=1) +
  ylab("Densidad") + ggtitle("n=100") + xlab(expression(hat(beta)[1])) + 
  theme_bw()

g31 = ggplot(betas) +
  geom_histogram(aes(betas[,5],y=..density..), col="black", bins = 30) +   #bins cantidad de barritas
  stat_function(fun = rnorm, args = list(mean=mean(betas[,5]), sd = sd(betas[,5])),
                geom = "line", col="red", size=1) +
  ylab("Densidad") + ggtitle("n=500") + xlab(expression(hat(beta)[1])) + 
  theme_bw()

g41 = ggplot(betas) +
  geom_histogram(aes(betas[,5],y=..density..), col="black", bins = 30) +   #bins cantidad de barritas
  stat_function(fun = rnorm, args = list(mean=mean(betas[,5]), sd = sd(betas[,8])),
                geom = "line", col="red", size=1) +
  ylab("Densidad") + ggtitle("n=1000") + xlab(expression(hat(beta)[1])) + 
  theme_bw()

grid.arrange(g11,g21,g31,g41)

#Pregunta 3.C
library(ggplot2)
library(dplyr)
library(gridExtra)

set.seed(150)
repeticiones=10000
betas=matrix(NA,nrow = repeticiones,ncol = 8)

beta0=2
beta1=2.5
beta2=1

su=1

n=c(50,100,500,1000)

for (j in 1:length(n)) {
  x1=rnorm(n[j],20,1)
  x2=rnorm(n[j],0,su)
  
  for (i in 1:repeticiones) {
    u=rnorm(n[j],0,su)
    Y0=beta0+beta1*x1+u
    Y1=beta0+beta1*x1+beta2*x2+u
    
    model0=lm(Y0~x1)
    betas[i,j]=model0$coef[2]
    
    model1=lm(Y1~x1+x2)
    betas[i,j+4]=model1$coef[2]
    
  }
  
}

betas_2=data.frame(betas)

apply(betas_2,2,mean)
apply(betas_2,2,var)

#3.C.C.B

g11=ggplot(betas_2) + geom_histogram(aes(betas_2[,5],y=..density..), col="black", bins = 30) +   #bins cantidad de barritas
  stat_function(fun = rnorm, args = list(mean=mean(betas_2[,5]), sd = sd(betas_2[,6])),
                geom = "line", col="red", size=1) +
  ylab("Densidad") + ggtitle("n=50") + xlab(expression(hat(beta)[1])) + 
  theme_bw()

g21 = ggplot(betas_2) +
  geom_histogram(aes(betas_2[,5],y=..density..), col="black", bins = 30) +   #bins cantidad de barritas
  stat_function(fun = rnorm, args = list(mean=mean(betas_2[,5]), sd = sd(betas_2[,7])),
                geom = "line", col="red", size=1) +
  ylab("Densidad") + ggtitle("n=100") + xlab(expression(hat(beta)[1])) + 
  theme_bw()

g31 = ggplot(betas_2) +
  geom_histogram(aes(betas_2[,5],y=..density..), col="black", bins = 30) +   #bins cantidad de barritas
  stat_function(fun = rnorm, args = list(mean=mean(betas_2[,5]), sd = sd(betas_2[,5])),
                geom = "line", col="red", size=1) +
  ylab("Densidad") + ggtitle("n=500") + xlab(expression(hat(beta)[1])) + 
  theme_bw()

g41 = ggplot(betas_2) +
  geom_histogram(aes(betas_2[,5],y=..density..), col="black", bins = 30) +   #bins cantidad de barritas
  stat_function(fun = rnorm, args = list(mean=mean(betas_2[,5]), sd = sd(betas_2[,8])),
                geom = "line", col="red", size=1) +
  ylab("Densidad") + ggtitle("n=1000") + xlab(expression(hat(beta)[1])) + 
  theme_bw()

grid.arrange(g11,g21,g31,g41)