## Welcome to GitHub Pages

You can use the [editor on GitHub](https://github.com/KiriBenitez/Tarea5/edit/master/README.md) to maintain and preview the content for your website in Markdown files.

Whenever you commit to this repository, GitHub Pages will run [Jekyll](https://jekyllrb.com/) to rebuild the pages in your site, from the content in your Markdown files.

### Markdown

Markdown is a lightweight and easy-to-use syntax for styling your writing. It includes conventions for

```markdown
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
  
  #En esta parte es un for por lo cual aqui hace las 10.000 repeticiones de acuerdo a los modelos que se dan
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
```

For more details see [GitHub Flavored Markdown](https://guides.github.com/features/mastering-markdown/).

### Jekyll Themes

Your Pages site will use the layout and styles from the Jekyll theme you have selected in your [repository settings](https://github.com/KiriBenitez/Tarea5/settings). The name of this theme is saved in the Jekyll `_config.yml` configuration file.

### Support or Contact

Having trouble with Pages? Check out our [documentation](https://help.github.com/categories/github-pages-basics/) or [contact support](https://github.com/contact) and we’ll help you sort it out.
