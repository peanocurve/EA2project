rm(list = ls())
library(lattice)
library(ggplot2)
library(nutshell)
library(tidyverse)
library(fastDummies)

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


#Análisis exploratorio
#Veamos que variables son reelevantes y normales

exploranba <- nba_raw %>% 
  select_if(is.numeric) %>% 
  select(-c(GP,W,L,MIN,`plus/minus`,FGM,FGA,`3PA`,`OREB`,`DREB`,`BLK`,`BLKA`,`FTM`,PFD))

pairs(exploranba)
summary(exploranba)
apply(exploranba,2,sd)
cor(exploranba)


par(mfrow=c(2,2))
for(i in 1:12){
  hist(exploranba[,i], xlab = colnames(exploranba)[i], ylab = "frecuencia", main = "", breaks = 20)
}

#Regresión

#Análisis de supuestos, varianzas, heterocedasticidad, colinealidad