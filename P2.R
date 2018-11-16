# PREGUNTA 2

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

# Funcion que grafica retornos
grafico_retorno=function(x){
  ggplot(x) + geom_line(mapping=aes(date, retornos.mensuales, color=symbol)) +
    labs(title="Retornos accionarios",
         x="Año", y="% retorno")
}

# Graficando retornos
grafico_retorno(retorno_activos)

# Funcion que calcula y grafica retornos acumulados
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

# Funcion que testea normalidad con Jarque Bera
  n<-(count(retornos))
  promedio<-as.numeric(mean(retornos))
  
  #skewness_a
  resultado_a <- vector()
  for(i in 1:n){
  resultado_a <- c(resultado_a,((retornos[i]-promedio)^3))
  }
  sk_a<-1/n*sum(resultado_a)
  #skewness_b
  resultado_b <- vector()
  for(i in 1:n){
  resultado_b <- c(resultado_b,((retornos[i]-promedio)^2))
  }
  sk_b<-(1/n*sum(resultado_b))^(3/2)
  #sk_final
  sk_final<-sk_a/sk_b
  
  #kurtosis_a
  resultado_ka <- vector()
  
  for(i in 1:n){
  resultado_ka <- c(resultado_ka,((retornos[i]-promedio)^4))
  }
  kurt_a<-1/n*sum(resultado_ka)
  
  #skewness_b
  resultado_kb <- vector()
  
  for(i in 1:n){
  resultado_kb <- c(resultado_kb,((retornos[i]-promedio)^2))
  }
  
  kurt_b<-(1/n*sum(resultado_kb))^(2)
  #sk_final
  kurt_final<-kurt_a/kurt_b
  jb<-n*(((sk_final^2)/6)+(((kurt_final-3)^2)/24))
  
  #contraste de hipótesis, si JB>X(a,2)^2 (5,99 a un nivel de significancia del 5%), entonces se rechaza la hipotesis nula
  if (jb>5.99) {print("Se rechaza la hipotesis nula. Los datos no siguen una distribucion normal")} 
  else{print("No se puede rechazar la hipótesis nula. Los datos siguen una distribución normal")}
