## Bienvenido a nuestro trabajo de R

Como grupo, quicieramos indicar que lo que está presente en este trabajo es resultado de horas de lucha intensa. De cualquier manera, adjuntamos al correo el R.script correspondiente y su R Markdown.

# Tarea 5

A continuación el código R Markdown

```markdown
title: "Tarea 5"
author: "Benítez - Canelo - Ortiz"
date: "16 de noviembre de 2018"
output:
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
## Pregunta 2

A continuacion se descargan los precios de las acciones de Microsoft("MSFT") y de Apple("AAPL")

```{r}

library(tidyquant)
library(tidyverse)
library(dplyr)

# Creando DF con precios de acciones

tickers=c("MSFT", "AAPL")
data_activos=tq_get(tickers,
                    get="stock.prices",
                    from="2000-01-01",
                    to="2018-08-31",
                    periodicity="monthly")
```

## 2.a. Se crea la funcion para calcular retornos:

```{r}

# Funcion que calcula retorno de activos
funcion_retornos=function(df){
  retorno_activos=df %>%
    group_by(symbol) %>%
    tq_transmute(select=close,
                 mutate_fun=periodReturn,
                 period="monthly",
                 type="log",
                 col_rename="retornos.mensuales")
  return(retorno_activos)
}
# Calculando retornos
retorno_activos=funcion_retornos(data_activos)
print(retorno_activos)

```

## 2.b. Se crea la función para graficar retornos:

```{r}

grafico_retorno=function(x){
  ggplot(x) + geom_line(mapping=aes(date, retornos.mensuales, color=symbol)) +
    labs(title="Retornos accionarios",
         x="Año", y="% retorno")
}

# Graficando retornos
grafico_retorno(retorno_activos)

```

## Función para calcular y graficar retornos acumulados:

```{r}

grafico_retorno_acum=function(x){
  retcum=x%>%
    mutate(cum_ret=cumsum(retornos.mensuales))
  
  ggplot(retcum) + geom_line(mapping=aes(date, cum_ret, color=symbol)) +
    labs(title="Retornos acumulados",
         x="Año", y="% retorno acumulado") +
    guides(fill=guide_legend(title="Acciones"))
}
# Graficando retornos acumulados
grafico_retorno_acum(retorno_activos)
```

## Pregunta 3

### Pregunta 3.a

A continuacion se mostra el codigo que se uso para resolver la pregunta.

```{r}
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

```

Como se puede ver en los resultados;

Medias

n= 50; x1= 2.498373 x5= 2.498540
n=100; x2= 2.500226 x6= 2.500004
n=500; x3= 2.500369 x7= 2.500364
n=1000;x4= 2.500255 x8= 2.500256

Varianza

n= 50; x1= 0.017026304  x5= 0.017085116
n=100; x2= 0.011075094  x6= 0.011273960
n=500; x3= 0.002032209  x7= 0.002035101
n=1000;x4= 0.001013762  x8= 0.001014232

Como se puede apreciar, los estimadores presentan sesgos en su estimacion, lo cual a medida que auqmentan e numero de variables se va reduciendo para ser igual a los estimadores poblacionales.


### Pregunta 3.b

A continuacion se presenta el grafico 

```{r}

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

betas=data.frame(betas)


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

```

### Pregunta 3.c

A continuacion se usara la variante donde x2 ??? U[0, 1], para luego proceder a hacer el mismo procedimiento que las partes a y b

### 3.C.A

```{r}
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


```

Como se puede ver en los resultados;

Medias

n= 50; x1= 2.498373 x5= 2.498540
n=100; x2= 2.500226 x6= 2.500004
n=500; x3= 2.500369 x7= 2.500364
n=1000;x4= 2.500255 x8= 2.500256

Varianza

n= 50; x1= 0.017026304  x5= 0.017085116
n=100; x2= 0.011075094  x6= 0.011273960
n=500; x3= 0.002032209  x7= 0.002035101
n=1000;x4= 0.001013762  x8= 0.001014232

Como se logra a apreciar presenta os mismos resultados que la variante de x2 = 0, 8x1 + e, lo cual se puede traducir que el estimador presenta sesgo, lo cual se ve disminuido a medida que aumenta el numero de observaciones

### 3.C.B

A continuacion mostramos el grafico de grilla de acuerdo a las nuevas indicaciones

```{r}

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

## 3.C.C.B

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
```

For more details see [GitHub Flavored Markdown](https://guides.github.com/features/mastering-markdown/).

### Jekyll Themes
