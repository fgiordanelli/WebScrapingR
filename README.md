library(rvest)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(XML)

corrida <- data.frame()
pareo <- data.frame()
guardar_novo <- data.frame()


for (j in c(442411760, 442361721,442351715,442341707,442291664,442281658,442271649,442221610,442211604,442201596,442151557,442141548,442131539,442081500,442071493,442061484,442001440,441931390,441911373,441871354,441861347,441851338,441801297,441791290,441781282,441731243,441721237,441711229,441661191,441651185,441641178,441591137,441581131,441571122,441521081,441511075,441501066,441451024,441441018,441431009,441380970,441360956,441350946,441310915,441300909,441280889,441230852,441220843,441170804,441160795,441150786,441100745,441090739 #441080731
            )){
  for (i in 1:11){
  
  url <- paste0("https://apostas.jcb.com.br/pt-br/corridas/resultadocompleto?idReuniao=",j,"&numeroPareo=",i)
  
  source <- readLines(url, encoding = "UTF-8")
  parsed_doc <- htmlParse(source, encoding = "UTF-8")
  
  encontrar <- xpathSApply(parsed_doc, path = '//*[@id="content"]/div/div[1]/div[2]/div[1]/div[2]/ul/li[1]/a/span[3]', xmlValue)
  
  modificar <- gsub(",", ".", encontrar, fixed = TRUE)
  
  guardar <- as.data.frame(unlist(strsplit(modificar,split='\n', fixed=TRUE)))
  

  corrida <- c(corrida,j)
  pareo <- c(pareo, i)
  guardar_novo <- c(guardar_novo,guardar)
  
  
  
  }
}

guardar_novo

guardar1 %>% View()

guardar <- guardar1 %>% filter(!guardar1[,1] %in% c("","Rateio")) %>% rename(Rateio = 1) 

corrida1 <- do.call(rbind.data.frame, corrida)
pareo1 <- do.call(rbind.data.frame, pareo)
guardar1 <- do.call(rbind.data.frame, guardar_novo)
colnames(guardar)[7]  <- "Rateio"
colnames(guardar)[23]  <- "Favorito"

guardar_novo2 <- guardar %>%  mutate(Favorito = ifelse(Favorito == "Favorito", "Favorito","Nao Favorito")) %>% select(Rateio, Favorito)

guardar_novo2[,1] <- gsub(",", ".", guardar_novo2[,1])

guardar_novo2$Rateio <- as.numeric(guardar_novo2$Rateio) 

guardar_novo3 <- guardar_novo2 %>%  mutate(ganhou = ifelse(Favorito == "Favorito",1,0)) %>% mutate(lucro = ifelse(Favorito == "Favorito",1*Rateio,-1)) 

guardar_novo3 %>% group_by(Favorito) %>% summarise(lucro = sum(lucro))
