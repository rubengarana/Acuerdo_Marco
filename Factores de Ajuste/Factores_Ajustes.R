

Tasa_Cambio <- function(BCN="https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/reportedeexcel.php?Fecha_inicial=2019-01-01&Fecha_final=2022-06-22") {
  
  library(lubridate)
  U <- url(BCN,method = "libcurl")
  T_C <- readLines(BCN)
  close(U)
  T_C <- data.frame(Cambio=T_C)
  T_C <- data.frame(Cambio=T_C[-c(1:3,nrow(T_C)),])
  T_C <- T_C[!T_C$Cambio=="</TR>",]
  T_C <- sub("<TR VALIGN=TOP><TD bgcolor='#E9E9E9'>","",T_C)
  T_C <- sub("<TD bgcolor='#E9E9E9'>","",T_C)
  T_C <- sub("</TD>","",T_C)
  T_C <- data.frame(Tasa=T_C)
  T_C$Fecha <- ifelse(grepl("-",T_C$Tasa),T_C$Tasa,NA)
  T_C$Fecha[is.na(T_C$Fecha)]<-T_C$Fecha[!is.na(T_C$Fecha)]
  T_C <- T_C[!grepl("-",T_C$Tasa),]
  T_C$Fecha <- dmy(T_C$Fecha)
  T_C$Tasa <- as.numeric(T_C$Tasa)
  row.names(T_C) <- 1:nrow(T_C)
  T_C <- T_C[,c(2,1)]
  
  return(T_C)
}

IPC <- function(URL = "https://www.inide.gob.ni/docs/ipc/ipc_2022/May2022/Cuadros_Estadisticas_IPC_Mayo_2022.xls"){
  
  library(tidyverse)
  library(readxl)
  download.file(URL,"C:/Users/rubena/Desktop/Acuerdos Marco/Acuerdo Marco/Factores de Ajuste/IPC.xls",
                method = "wininet",cacheOK = FALSE,mode = "wb")
  BIPC <- read_xls("C:/Users/rubena/Desktop/Acuerdos Marco/Acuerdo Marco/Factores de Ajuste/IPC.xls",sheet = 1,skip =117)
  BIPC <- BIPC[,-c(2,4:ncol(BIPC))]
  BIPC <- BIPC[!is.na(BIPC$`199.621522`),]
  BIPC <- BIPC %>% rename(mes=Diciembre) %>% rename(IPC=`199.621522`)
  BIPC$year <- ifelse(grepl("2",BIPC$mes),BIPC$mes,NA)
  BIPC <- BIPC %>% fill(year,.direction="down") %>% 
    unite(col="year-mes",year,mes,sep="-",remove=FALSE) %>%
    filter(!grepl("2",BIPC$mes)) %>% select(-c("mes","year")) %>%
    mutate(Crecimiento=as.numeric(BIPC[nrow(BIPC),"IPC"])/IPC)
  
  return(BIPC)
  
}

I_Importaciones <- function(path="C:/Users/rubena/Desktop/Acuerdos Marco/Acuerdo Marco/Factores de Ajuste/Indice_importaciones.xlsx"){
  library(readxl)
  library(tidyverse)
  IMP <- read_xlsx(path,sheet = 1,skip = 1)
  IMP$Mes <- str_to_title(IMP$Mes)
  IMP <- IMP %>% unite("year-mes",1,2,sep="-") %>% 
    mutate(Indice_C=as.numeric(IMP[nrow(IMP),"Indice"])/Indice)
  return(IMP)
}