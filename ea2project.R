rm(list = ls())
library(lattice)
library(ggplot2)
library(tidyverse)
library(fastDummies)
library(corrplot)
library(readxl)


ye#Lectura y limpieza de datos
read_nba <- function(path){
  path %>% 
    read_excel() %>% 
    mutate(Year = str_extract_all(path, 'Documents/GitHub/EA2project/NBASEASONSUMMARY//NBA|\\d+-\\d+|.xlsx')[[1]][2])
}

divconf<-read_excel("Documents/GitHub/EA2project/conference-division.xlsx")

nba_raw <- dir("Documents/GitHub/EA2project/NBASEASONSUMMARY/",full.names = T) %>%
  map_df(read_nba) 

nba <- nba_raw %>%
  left_join(divconf, by = c('TEAM' = 'team') ) %>% 
  dummy_cols(select_columns = 'conference') %>% 
  dummy_cols(select_columns = 'division') %>%
  dummy_cols(select_columns = 'Year')


##Análisis exploratorio
#Cantidad de triples lanzados promedio de las últimas 10 temporadas y de puntos
mediatriples<-aggregate(nba[,12], list(nba$Year), mean)
ggplot(data=mediatriples, aes(x=mediatriples$Group.1, y=mediatriples$`3PA`)) + geom_bar(stat="identity", width=0.3) +ggtitle('Triples intentados promedio por temporada')+labs(x='Temporada',y="Triples intentados por partido")
mediapuntos<-aggregate(nba[,7], list(nba$Year), mean)
ggplot(data=mediapuntos, aes(x=mediapuntos$Group.1, y=mediapuntos$PTS)) + geom_bar(stat="identity", width=0.3) +ggtitle('Puntos promedio por partido cada temporada')+labs(x='Temporada',y="Puntos por partido promedio")
ggplot(nba,aes(x=nba$`3PA`, y=nba$PTS)) +geom_point()
ptsreg<-lm(PTS~`3PA`, data=nba)
summary(ptsreg)
plot(ptsreg)

#regresion plus/minus vs win%

rwplus<-lm(`WIN%`~`plus/minus`,data=nba)
ggplot(nba,aes(y=nba$`WIN%`, x=nba$`plus/minus`)) +geom_point()+labs(y='Porcentaje de juegos ganados', x='plus/minus')
summary(rwplus)

#Veamos que variables son reelevantes 

exploranba <- nba_raw %>% 
  select_if(is.numeric) %>% 
  select(-c(GP,W,L,MIN,`plus/minus`,OREB,DREB))
  
pairs(exploranba)
summary(exploranba)

exploranba <- nba_raw %>% 
  select_if(is.numeric) %>% 
  select(-c(GP,W,L,MIN,`plus/minus`,FGM,FGA,`3PM`,`OREB`,`DREB`,`BLK`,`BLKA`,`FTM`,PFD,PF,AST))
pairs(exploranba)

#Varianza de variebles de interés
apply(exploranba,2,sd)

#Explorando la importancia de pertenecer a determinada conferencia
ggplot(nba, aes(`WIN%`*100, geom = "density", fill = conference)) + 
  geom_histogram(bins= 100) + 
  facet_grid(conference~.) + 
  labs(title="Distribucion del porcentaje de victorias por conferencia",
       y = "Conteo",
       x = "Porcentaje")

ggplot(data = nba, aes(x = conference, y =`WIN%`*100 , fill = conference)) + 
  geom_boxplot() + 
  labs(y = "Porcentaje de victorias", x = "Conferencia")

#Correlacion de variables de interes
corrplot(cor(exploranba, use = "complete.obs"), is.corr = T, 
         method = "ellipse", diag = T,  addCoefasPercent = F, tl.col = "blue",addCoef.col = "red")

ggplot(nba, aes(x=`WIN%`))+ geom_histogram(color="darkblue", fill="lightblue", bins =100)

stars(exploranba,key.loc=c(-0.7,17))

#Resumen exlploranba
summary(exploranba)

#Indicador, suponiendo el modelo, que los errores son normales
qqnorm(exploranba$`WIN%`)

#Regresión e inferencia considerando la conferncia
wregconf<-lm(`WIN%`~ (`FG%`+PTS+`3PA` +`3P%` + FTA + `FT%` + REB + TOV + STL+ conference_Eastern), data=nba)
summary(wregconf)

#Regresión e inferencia sin considerar la conferencia
wreg<-lm(`WIN%`~ (`FG%`+PTS+`3PA`+`3P%` +FTA + `FT%` + REB+ TOV+ STL), data=nba)
summary(wreg)
plot(wreg)

#Regresión utilizando un término cuadrático
wreg<-lm(`WIN%`~ (`FG%`+I(PTS^2)+PTS+`3PA`+`3P%` +FTA + `FT%` + REB + TOV+ STL ), data=nba)
summary(wreg)
plot(wreg)

#Regresión e inferencia considerando el año y la conferencia
#(wreg<-lm(`WIN%`~ (`FG%`+`3PA`+`3P%` +FTA + `FT%` + DREB+ TOV+ STL+`Year_2010-2011`+`Year_2011-2012`+`Year_2012-2013`+`Year_2013-2014`+`Year_2014-2015`+`Year_2015-2016`+`Year_2016-2017`+`Year_2017-2018`+`Year_2018-2019`+`conference_Eastern`), data=nba)

#Análisis de supuestos
#ANNOVA
anovawreg<-aov(wreg)
summary(aov(wreg))
boxplot(wreg$residuals)
plot(anovawreg)

#colinealidad
pairs(exploranba)

#normalidad de residuos
qqnorm(wreg$residuals)

#Observemos la homocedasticidad
ggplot(nba,aes(x=nba$`FG%`, y=wreg$residuals)) +geom_point()
ggplot(nba,aes(x=nba$`PTS`, y=wreg$residuals)) +geom_point()
ggplot(nba,aes(x=nba$`3PM`, y=wreg$residuals)) +geom_point()
ggplot(nba,aes(x=nba$`3P%`, y=wreg$residuals)) +geom_point()
ggplot(nba,aes(x=nba$`FTA`, y=wreg$residuals)) +geom_point()
ggplot(nba,aes(x=nba$`FT%`, y=wreg$residuals)) +geom_point()
ggplot(nba,aes(x=nba$`REB`, y=wreg$residuals)) +geom_point()
ggplot(nba,aes(x=nba$`TOV`, y=wreg$residuals)) +geom_point()
ggplot(nba,aes(x=nba$`STL`, y=wreg$residuals)) +geom_point()

