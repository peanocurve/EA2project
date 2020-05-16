rm(list = ls())
library(lattice)
library(ggplot2)
library(nutshell)
library(tidyverse)
library(fastDummies)
library(corrplot)

#Lectura y limpieza de datos
read_nba <- function(path){
  path %>% 
    read_xlsx() %>% 
    mutate(Year = str_extract_all(path, 'Documents/GitHub/EA2project/NBASEASONSUMMARY//NBA|\\d+-\\d+|.xlsx')[[1]][2])
}

divconf<-read_xlsx("Documents/GitHub/EA2project/conference-division.xlsx")

nba_raw <- dir("Documents/GitHub/EA2project/NBASEASONSUMMARY/",full.names = T) %>%
  map_df(read_nba) 

nba <- nba_raw %>%
  left_join(divconf, by = c('TEAM' = 'team') ) %>% 
  dummy_cols(select_columns = 'conference') %>% 
  dummy_cols(select_columns = 'division')


##Análisis exploratorio

#Veamos que variables son reelevantes 

exploranba <- nba_raw %>% 
  select_if(is.numeric) %>% 
  select(-c(GP,W,L,MIN,`plus/minus`))
  
pairs(exploranba)
summary(exploranba)

exploranba <- nba_raw %>% 
  select_if(is.numeric) %>% 
  select(-c(GP,W,L,MIN,`plus/minus`,FGM,FGA,`3PA`,`OREB`,`DREB`,`BLK`,`BLKA`,`FTM`,PFD,PF,AST))

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

#Indicador, suponiendo el modelo, que los errores son normales
qqnorm(exploranba$`WIN%`)

#Regresión e inferencia
wreg<-lm(`WIN%`~ (`FG%`+PTS+`3PM` +`3P%` + FTA + `FT%` + REB + TOV + STL), data=nba)
summary(wreg)

#Análisis de supuestos
#ANNOVA
summary(aov(wreg))
boxplot(wreg$residuals)

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
