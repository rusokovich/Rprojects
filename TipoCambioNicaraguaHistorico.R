#Limpiar el area de trabajo 
rm(list=ls())


# required libraries  
library(remotes)
library(rJava)
library(tidyverse)
library(dplyr)
library(tidyr)
library(tabulizer)
library(lubridate)
library(RSelenium)
library(rvest)

# install the packages
#remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"))

# more information:https://github.com/ropensci/tabulizer


# load the pdf 
# set the work directory 

nombre_meses <- c("enero", "febrero", "marzo", "abril", "mayo", "junio",
                  "julio", "agosto", "septiembre", "octubre", 
                  "noviembre", "diciembre")

tabla_meses <- data.frame(NombreMes = nombre_meses, NumeroMes = 1:12)


# funcion generadora de fechas como un string
generate_date_sequences <- function(start) {
  start_date <- as.Date(paste(start, "01-01", sep = "-"), format = "%Y-%m-%d")
  end_date <- as.Date(paste(start, "12-31", sep = "-"), format = "%Y-%m-%d")
  return(as.character(seq.Date(start_date, end_date, by = "days")))
}


# funcion transformadora de dataframes

transformacion_table_tipo_cambio <- function(tabla_tipo_cambio) {

tabla_tipo_cambio<-pivot_longer(tabla_tipo_cambio, cols =-dia
                                ,names_to = "mes",values_to = "valor")

tabla_tipo_cambio <- tabla_tipo_cambio %>%
  mutate(ano = gsub("\\D", "",sub(".*/([^/]+)$", "\\1", url))) %>% 
  left_join(tabla_meses, by=c("mes"="NombreMes"))

tabla_tipo_cambio$fechastring <- paste(tabla_tipo_cambio$dia
                                       ,as.character(tabla_tipo_cambio$NumeroMes)
                                       ,tabla_tipo_cambio$ano,sep="-")

tabla_tipo_cambio$fecha <- as.Date(tabla_tipo_cambio$fechastring, format = "%d-%m-%Y")

tabla_tipo_cambio<- tabla_tipo_cambio %>% 
  select(-c(dia,mes,ano,NumeroMes,fechastring)) %>%
  select(fecha,valor) %>%  rename(TipoCambio = valor)


tabla_tipo_cambio$TipoCambio<-as.numeric(tabla_tipo_cambio$TipoCambio)

tabla_tipo_cambio<-na.omit(tabla_tipo_cambio)
}

# establecer el area de trabajo

setwd('path')



# ---- Automatizacion de la extraccion ----- 
# https://www.youtube.com/watch?v=Dkm1d4uMp34 mayor referencia de RSelenium

remote.driver <- rsDriver(browser="chrome"
                          ,chromever = "119.0.6045.105"
                          ,verbose = F
                          ,port = free_port()) 


# crear el cliente

remDr<-remote.driver$client


# abrir el browser

remDr$open()
remDr$navigate("https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/index.php?&val=0")


butonListAnos <- remDr$findElement(using = "xpath", value = "//*[@id='frmHistory']/p/select")
butonListAnos$clickElement()

select_element <- remDr$findElement("name", "lstYear")

# encontrar el tag the option
option_elements <- select_element$findChildElements("tag name", "option")

# contar las opciones
number_of_options <- length(option_elements)


# el Extraer los URLs y los nombres

lista_urls <- c()
lista_urls_iteracion<- c()
df_urls <-data.frame()
tabla_tipo_cambio <-data.frame()

for (numerodeopiones in 1:number_of_options) {
print(numerodeopiones)
  
  primeraOpcion_butonListAnos <- remDr$findElement(using = "xpath", value = paste("//*[@id='frmHistory']/p/select/option[",numerodeopiones,"]"))   
  primeraOpcion_butonListAnos$clickElement()
  
  butonIr <- remDr$findElement(using = "xpath", value = "//*[@id='frmHistory']/p/input")   
  butonIr$clickElement()
  
  
  # guardar el html fuente
  presente_url <- read_html(remDr$getCurrentUrl()[[1]])
  
  # Extaer todas los URLs
  href_urls <- presente_url %>%
    html_nodes("a") %>%
    html_attr("href")
  

   pdf_urls <- href_urls[grep("\\.pdf$", href_urls)]
  
  lista_urls_iteracion<-gsub("0212/\\.\\./", "",paste0(remDr$getCurrentUrl()[[1]],pdf_urls))
  
  temp_df <- data.frame(nombreArchivo = pdf_urls, urls = lista_urls_iteracion)
  
  df_urls <- rbind(df_urls,temp_df)
  
  
  # lista_urls <- c(lista_urls, lista_urls_iteracion)
  # print(lista_urls)
  # lista_urls_iteracion <- c()
}




# ***********  crear la base de datos *******************

# ***** Descargar los archivos opcional **************

# recordar se van a guardar en el area de trabajo definida

# for (i in nrow(df_urls):1) {
# 
#     url<-df_urls[i,2]
#     print(paste0('Historico_', df_urls[i,1]))
#     print(paste0('Historico_', gsub("\\.\\./20[0-2][0-9]/tipcamb", "", df_urls[i,1])))
#     # descargar los archivos
#     download.file(url, paste0('Historico_', gsub("\\.\\./20[0-2][0-9]/", "", df_urls[i,1])), mode="wb")
# 
# }


tabla_tipo_cambio_nicaragua<- data.frame()
tabla_tipo_cambio<- data.frame()
# crear la base de datos tipo de cambio en nicaragua
for (i in nrow(df_urls):1) {

  
  print(df_urls[i,1])

if (df_urls[i,1] == "1931-1984.pdf") {
  
remDr$navigate("https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/2007/tipcamb2007.pdf")

url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/3193/1931-1984.pdf"

# para un archivo 
# extract_tables(tipo_cambio_nicaragua) %>% as.data.frame()

#Locate the areas
#locate_areas(url)

tabla_tipo_cambio<-extract_tables(url,guess = TRUE) %>% 
  as.data.frame() %>%
  rename(
    ano = X1, enero = X2, febrero = X3, marzo = X4,
    abril = X5,mayo = X6, junio = X7, julio = X8, agosto = X9,
    septiembre = X10, octubre = X11, noviembre = X12,
    diciembre = X13, promedio = X14
  )

colnames(tabla_tipo_cambio)
# remover la columna promedio
tabla_tipo_cambio<-tabla_tipo_cambio[,-which(names(tabla_tipo_cambio) == "promedio")]

# crear la tabla inversa 
tabla_tipo_cambio<-pivot_longer(tabla_tipo_cambio, cols =-ano
                                 ,names_to = "mes",values_to = "valor")

tabla_tipo_cambio$check.ano<-nchar(tabla_tipo_cambio$ano)

# usando dplyr
tabla_tipo_cambio_regulares <- tabla_tipo_cambio %>% filter(check.ano == 4)


# ----- manejo de irregularidades --------------
tabla_tipo_cambio_irregulares <- tabla_tipo_cambio %>% filter(check.ano != 4)

# encontrar los anos
tabla_tipo_cambio_irregulares$inicio.serie<-substr(tabla_tipo_cambio_irregulares$ano, 1, 4) 

tabla_tipo_cambio_irregulares$final.serie<-paste("19",substr(tabla_tipo_cambio_irregulares$ano
                                                             ,unlist(gregexpr("-", tabla_tipo_cambio_irregulares$ano))+1
                                                             ,nchar(tabla_tipo_cambio_irregulares$ano)),sep="")

# convertirlos a numeros
tabla_tipo_cambio_irregulares$inicio.serie<-as.numeric(tabla_tipo_cambio_irregulares$inicio.serie)
tabla_tipo_cambio_irregulares$final.serie<-as.numeric(tabla_tipo_cambio_irregulares$final.serie)

# crear las secuencias de los anos 
tabla_tipo_cambio_irregulares$anios.cuestion <- mapply(function(start, end) {
  seq(start, end, by = 1)
}, tabla_tipo_cambio_irregulares$inicio.serie, tabla_tipo_cambio_irregulares$final.serie)

# unir con la tabla meses para obtener el numero de mes 
tabla_tipo_cambio_irregulares <- tabla_tipo_cambio_irregulares %>%
  unnest(anios.cuestion) %>% select(-c(inicio.serie, final.serie, check.ano,ano)) %>%
  left_join(tabla_meses, by=c("mes"="NombreMes"))




# aplicar la funcion a cada una de las filas 
tabla_tipo_cambio_irregulares <- tabla_tipo_cambio_irregulares %>%
  mutate(fechas.cada.ano = map(anios.cuestion, generate_date_sequences))

# expandir las fechas
tabla_tipo_cambio_irregulares <- tabla_tipo_cambio_irregulares %>%
  unnest(fechas.cada.ano)

# extraer solo las fechas necesarias
tabla_tipo_cambio_irregulares$numero.ano <- year(tabla_tipo_cambio_irregulares$fechas.cada.ano)
tabla_tipo_cambio_irregulares$numero.mes <- month(tabla_tipo_cambio_irregulares$fechas.cada.ano)
tabla_tipo_cambio_irregulares$check.fechas <- tabla_tipo_cambio_irregulares$numero.ano == tabla_tipo_cambio_irregulares$anios.cuestion&tabla_tipo_cambio_irregulares$NumeroMes == tabla_tipo_cambio_irregulares$numero.mes

#str(tabla_tipo_cambio_irregulares)

tabla_tipo_cambio_irregulares<-
  tabla_tipo_cambio_irregulares[tabla_tipo_cambio_irregulares$check.fechas == TRUE,] %>% 
  select(c(fechas.cada.ano,valor))


#str(tabla_tipo_cambio_irregulares)

# ----- manejo de regularidades  --------------


tabla_tipo_cambio_regulares <- tabla_tipo_cambio_regulares %>%
  select(-c(check.ano)) %>%
  left_join(tabla_meses, by=c("mes"="NombreMes"))


# aplicar la funcion a cada una de las filas 
tabla_tipo_cambio_regulares <- tabla_tipo_cambio_regulares %>%
  mutate(fechas.cada.ano = map(ano, generate_date_sequences))

# expandir las fechas
tabla_tipo_cambio_regulares <- tabla_tipo_cambio_regulares %>%
  unnest(fechas.cada.ano)

# extraer solo las fechas necesarias
tabla_tipo_cambio_regulares$numero.ano <- year(tabla_tipo_cambio_regulares$fechas.cada.ano)
tabla_tipo_cambio_regulares$numero.mes <- month(tabla_tipo_cambio_regulares$fechas.cada.ano)
tabla_tipo_cambio_regulares$check.fechas <- tabla_tipo_cambio_regulares$numero.ano == tabla_tipo_cambio_regulares$ano & tabla_tipo_cambio_regulares$NumeroMes == tabla_tipo_cambio_regulares$numero.mes

tabla_tipo_cambio_regulares<-
  tabla_tipo_cambio_regulares[tabla_tipo_cambio_regulares$check.fechas == TRUE,] %>% 
  select(c(fechas.cada.ano,valor))



#str(tabla_tipo_cambio_regulares)


# ---- datos historicos ------ 

tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_regulares, tabla_tipo_cambio_irregulares)



tabla_tipo_cambio_nicaragua$valor<-as.numeric(tabla_tipo_cambio_nicaragua$valor)


tabla_tipo_cambio_nicaragua$fechas.cada.ano<-as.Date(tabla_tipo_cambio_nicaragua$fechas.cada.ano)
colnames(tabla_tipo_cambio_nicaragua)<-c("fecha","TipoCambio")

#str(tabla_tipo_cambio_nicaragua)

print("Fin de la primera iteracion")

} 
  else if (df_urls[i,1] =="1988.pdf") {
    #url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/3193/1988.pdf"
    
    url<- df_urls[i,2]
    
    #Locate the areas
    #locate_areas(url)
    
    
    tabla_tipo_cambio<-extract_tables(url,guess = FALSE
                                      ,area = list(c(163.3037,106.7701,484.9342,724.5927))
    ) %>% 
      as.data.frame()%>% slice(1:(n() - 2)) %>% select(-X2,-X3,-X12) %>% 
      mutate(X1 = str_replace_all(X1, "1\\)", ""))  %>%
      mutate(X1 = str_squish(X1)) %>%
      separate(X1, into = c("X1","X2","X3"), sep = " ") %>% 
      rename(
        dia = X1, enero = X2, febrero = X3, marzo = X4,
        abril = X5,mayo = X6, junio = X7, julio = X8, agosto = X9,
        septiembre = X10, octubre = X11, noviembre = X13,
        diciembre = X14, promedio = X15
      )
    
    
    # remover la columna promedio
    tabla_tipo_cambio<-tabla_tipo_cambio[,-which(names(tabla_tipo_cambio) == "promedio")]
    
    # crear la tabla inversa
    tabla_tipo_cambio<-pivot_longer(tabla_tipo_cambio, cols =-dia
                                    ,names_to = "mes",values_to = "valor")
    
    #tabla_tipo_cambio$valor<- gsub(",", "", tabla_tipo_cambio$valor)
    
    tabla_tipo_cambio <- tabla_tipo_cambio %>%
      mutate(ano = gsub("\\D", "",sub(".*/([^/]+)$", "\\1", url))) %>%
      left_join(tabla_meses, by=c("mes"="NombreMes"))
    
    tabla_tipo_cambio$fechastring <- paste(tabla_tipo_cambio$dia
                                           ,as.character(tabla_tipo_cambio$NumeroMes)
                                           ,tabla_tipo_cambio$ano,sep="-")
    
    tabla_tipo_cambio$fecha <- as.Date(tabla_tipo_cambio$fechastring, format = "%d-%m-%Y")
    
    
    tabla_tipo_cambio<- tabla_tipo_cambio %>%
      select(-c(dia,mes,ano,NumeroMes,fechastring)) %>%
      select(fecha,valor) %>%  rename(TipoCambio = valor)
    
    tabla_tipo_cambio$TipoCambio<-as.numeric(tabla_tipo_cambio$TipoCambio)
    
    tabla_tipo_cambio<-na.omit(tabla_tipo_cambio)
    
    
    #   Enero 1 a Febrero 14 de 1991: 70
    
    
    tabla_tipo_cambio$TipoCambio[tabla_tipo_cambio$fecha >= as.Date("1988-01-01") & tabla_tipo_cambio$fecha <= as.Date("1988-02-14")] <- 70
    
    
    tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)
  }
  
  
  else if (df_urls[i,1] == "1989.pdf") {
    #url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/3193/1989.pdf"
    
    url<- df_urls[i,2]
    
    #Locate the areas
    #locate_areas(url)
    
    
    tabla_tipo_cambio<-extract_tables(url,guess = FALSE
                                      ,area = list(c(163.3037,106.7701,484.9342,724.5927))
    ) %>% 
      as.data.frame()  %>% rename(
        X14 = X13
      ) %>% separate(X12, into = c("X12","X13"), sep = " ") %>% 
      rename(
        dia = X1, enero = X2, febrero = X3, marzo = X4,
        abril = X5,mayo = X6, junio = X7, julio = X8, agosto = X9,
        septiembre = X10, octubre = X11, noviembre = X12,
        diciembre = X13, promedio = X14
      ) %>% slice(-1)
    
    
    # remover la columna promedio
    tabla_tipo_cambio<-tabla_tipo_cambio[,-which(names(tabla_tipo_cambio) == "promedio")]
    
    # crear la tabla inversa
    tabla_tipo_cambio<-pivot_longer(tabla_tipo_cambio, cols =-dia
                                    ,names_to = "mes",values_to = "valor")
    
    tabla_tipo_cambio$valor<- gsub(",", "", tabla_tipo_cambio$valor)
    
    tabla_tipo_cambio <- tabla_tipo_cambio %>%
      mutate(ano = gsub("\\D", "",sub(".*/([^/]+)$", "\\1", url))) %>%
      left_join(tabla_meses, by=c("mes"="NombreMes"))
    
    tabla_tipo_cambio$fechastring <- paste(tabla_tipo_cambio$dia
                                           ,as.character(tabla_tipo_cambio$NumeroMes)
                                           ,tabla_tipo_cambio$ano,sep="-")
    
    tabla_tipo_cambio$fecha <- as.Date(tabla_tipo_cambio$fechastring, format = "%d-%m-%Y")
    
    
    tabla_tipo_cambio<- tabla_tipo_cambio %>%
      select(-c(dia,mes,ano,NumeroMes,fechastring)) %>%
      select(fecha,valor) %>%  rename(TipoCambio = valor)
    
    tabla_tipo_cambio$TipoCambio<-as.numeric(tabla_tipo_cambio$TipoCambio)
    
    tabla_tipo_cambio<-na.omit(tabla_tipo_cambio)
    
    tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)
  }
  
  else if (df_urls[i,1] == "1990.pdf") { 
    
    # 1990 
    #url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/3193/1990.pdf"
    
    url <- df_urls[i,2]
    
    tabla_tipo_cambio<-extract_tables(url,guess = FALSE, 
                                      area = list(c(174.0037,97.9143,460.6206,696.6491))) %>% 
      as.data.frame() %>%
      rename(
        dia = X1, enero = X2, febrero = X3, marzo = X4,
        abril = X5,mayo = X6, junio = X7, julio = X8, agosto = X9,
        septiembre = X10, octubre = X11, noviembre = X12,
        diciembre = X13, promedio = X14
      )
    
    # colnames(tabla_tipo_cambio)
    # remover la columna promedio
    tabla_tipo_cambio<-tabla_tipo_cambio[,-which(names(tabla_tipo_cambio) == "promedio")]
    
    # crear la tabla inversa 
    tabla_tipo_cambio<-pivot_longer(tabla_tipo_cambio, cols =-dia
                                    ,names_to = "mes",values_to = "valor")
    
    
    # aplicar la funcion a cada una de las filas 
    tabla_tipo_cambio <- tabla_tipo_cambio %>%  
      mutate(ano = gsub("\\D", "",sub(".*/([^/]+)$", "\\1", url))) %>% left_join(tabla_meses, by=c("mes"="NombreMes"))
    
    tabla_tipo_cambio$fechastring <- paste(tabla_tipo_cambio$dia
                                           ,as.character(tabla_tipo_cambio$NumeroMes)
                                           ,tabla_tipo_cambio$ano,sep="-")
    
    tabla_tipo_cambio$fecha <- as.Date(tabla_tipo_cambio$fechastring, format = "%d-%m-%Y")
    
    tabla_tipo_cambio$valor<- gsub(",", "", tabla_tipo_cambio$valor)
    
    
    tabla_tipo_cambio<- tabla_tipo_cambio %>% 
      select(-c(dia,mes,ano,NumeroMes,fechastring)) %>%
      select(fecha,valor) %>%  rename(TipoCambio = valor)
    # tabla_tipo_cambio$valor<-as.numeric(gsub(",", "", tabla_tipo_cambio$valor))
    
    tabla_tipo_cambio<-na.omit(tabla_tipo_cambio)
    
    tabla_tipo_cambio$TipoCambio<-as.numeric(tabla_tipo_cambio$TipoCambio)
    
    tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)
    
    
  } 
  
  else if (df_urls[i,1] == "1991.pdf") { 
    
    # 1991
    
    #url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/3193/1991.pdf"
    
    url <- df_urls[i,2]
    
    # este require transformaciones adicionales debido a un cambio en la politica monetaria
    
    # noviembre parece que no esta 
    
    tabla_tipo_cambio<-extract_tables(url,guess = FALSE, 
                                      area = list(c(152.8428,172.0801,406.0711,617.9060  ))) %>% 
      as.data.frame() %>%
      rename(
        dia = X1, enero = X2, febrero = X3, marzo = X4,
        abril = X5,mayo = X6, junio = X7, julio = X8, agosto = X9,
        septiembre = X10, octubre = X11, diciembre = X12,
        promedio= X13
      )
    
    # remover la columna promedio
    tabla_tipo_cambio<-tabla_tipo_cambio[,-which(names(tabla_tipo_cambio) == "promedio")]
    
    # crear la tabla inversa 
    tabla_tipo_cambio<-pivot_longer(tabla_tipo_cambio, cols =-dia
                                    ,names_to = "mes",values_to = "valor")
    
    # remover los 1) del texto
    tabla_tipo_cambio$valor <- gsub("1\\)", "", tabla_tipo_cambio$valor)
    
    
    # aplicar la funcion a cada una de las filas y agregar el ano 
    tabla_tipo_cambio <- tabla_tipo_cambio %>%
      mutate(ano = gsub("\\D", "",sub(".*/([^/]+)$", "\\1", url))) %>% left_join(tabla_meses, by=c("mes"="NombreMes"))
    
    tabla_tipo_cambio$fechastring <- paste(tabla_tipo_cambio$dia
                                           ,as.character(tabla_tipo_cambio$NumeroMes)
                                           ,tabla_tipo_cambio$ano,sep="-")
    
    tabla_tipo_cambio$fecha <- as.Date(tabla_tipo_cambio$fechastring, format = "%d-%m-%Y")
    
    
    # transformacion necesaria para poder convertirlos en numeros 
    
    # tabla_tipo_cambio$valor<- gsub(",", "", tabla_tipo_cambio$valor)
    
    
    
    tabla_tipo_cambio<- tabla_tipo_cambio %>% 
      select(-c(dia,mes,ano,NumeroMes,fechastring)) %>%
      select(fecha,valor) %>%  rename(TipoCambio = valor)
    
    tabla_tipo_cambio$TipoCambio<-as.numeric(tabla_tipo_cambio$TipoCambio)
    
    
    
    
    # Nota: Para efectos de calcular el tipo de cambio promedio se aplicó la corrección monetaria del 3 de marzo/91 en enero, febrero hasta el 2 de marzo/91. El córdoba oro se devaluó
    # pasando a valer C$5.00 por USD1.00 y esto equivalía a C$5,000,000.00 (córdobas viejos), y vuelve a llamarse córdoba. A partir del 30 de abril/91 el córdoba reemplaza a los
    # viejos, y estableciéndose como única moneda de curso legal.
    # Los tipos de cambio reales fueron los siguientes:
    #   Enero del 1 al 6 de 1991: 3,000,000.00
    
    tabla_tipo_cambio$TipoCambio[tabla_tipo_cambio$fecha >= as.Date("1991-01-01") & tabla_tipo_cambio$fecha <= as.Date("1991-01-06")] <- 3000000
    
    #   Enero del 7 al 13 de 1991: 3,210,000.00
    
    tabla_tipo_cambio$TipoCambio[tabla_tipo_cambio$fecha >= as.Date("1991-01-07") & tabla_tipo_cambio$fecha <= as.Date("1991-01-13")] <- 3210000
    
    #   Enero del 14 al 20 de 1991: 3,430,000.00
    
    tabla_tipo_cambio$TipoCambio[tabla_tipo_cambio$fecha >= as.Date("1991-01-14") & tabla_tipo_cambio$fecha <= as.Date("1991-01-20")] <- 3430000
    
    
    
    #   Enero del 21 al 27 de 1991: 3,740,000.00
    
    tabla_tipo_cambio$TipoCambio[tabla_tipo_cambio$fecha >= as.Date("1991-01-21") & tabla_tipo_cambio$fecha <= as.Date("1991-01-27")] <- 3740000
    
    
    #   Enero del 28 al 31 de 1991: 4,100,000.00
    tabla_tipo_cambio$TipoCambio[tabla_tipo_cambio$fecha >= as.Date("1991-01-28") & tabla_tipo_cambio$fecha <= as.Date("1991-01-31")] <- 4100000
    
    #   Febrero del 1 al 3 de 1991: 4,100,000.00
    tabla_tipo_cambio$TipoCambio[tabla_tipo_cambio$fecha >= as.Date("1991-02-01") & tabla_tipo_cambio$fecha <= as.Date("1991-02-03")] <- 4100000
    
    #   Febrero del 4 al 10 de 1991: 4,400,000.00
    
    tabla_tipo_cambio$TipoCambio[tabla_tipo_cambio$fecha >= as.Date("1991-02-04") & tabla_tipo_cambio$fecha <= as.Date("1991-02-10")] <- 4400000
    
    #   Febrero del 11 al 17 de 1991: 4,700,000.00
    
    tabla_tipo_cambio$TipoCambio[tabla_tipo_cambio$fecha >= as.Date("1991-02-11") & tabla_tipo_cambio$fecha <= as.Date("1991-02-17")] <- 4700000
    
    #   Febrero del 18 al 24 de 1991: 4,950,000.00
    
    tabla_tipo_cambio$TipoCambio[tabla_tipo_cambio$fecha >= as.Date("1991-02-18") & tabla_tipo_cambio$fecha <= as.Date("1991-02-24")] <- 4950000
    
    #   Febrero del 25 al 28 de 1991: 5,200,000.00
    
    tabla_tipo_cambio$TipoCambio[tabla_tipo_cambio$fecha >= as.Date("1991-02-25") & tabla_tipo_cambio$fecha <= as.Date("1991-02-28")] <- 5200000
    
    #   Marzo del 1 al 2 de 1991: 5,200,000.00
    
    tabla_tipo_cambio$TipoCambio[tabla_tipo_cambio$fecha >= as.Date("1991-03-01") & tabla_tipo_cambio$fecha <= as.Date("1991-03-02")] <- 5200000
    
    tabla_tipo_cambio<-na.omit(tabla_tipo_cambio)
    
    tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)

  }
  
  else if (df_urls[i,1] == "1992.pdf") {
    
    # 1992 todo bien 
    
    # url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/3193/1992.pdf"
    
    url <- df_urls[i,2]
    
    # este require transformaciones adicionales debido a un cambio en la politica monetaria
    
    # noviembre parece que no esta 
    
    tabla_tipo_cambio<-extract_tables(url,guess = TRUE) %>% 
      as.data.frame() %>%
      rename(
        dia = X1, enero = X2, febrero = X3, marzo = X4,
        abril = X5,mayo = X6, junio = X7, julio = X8, agosto = X9,
        septiembre = X10, octubre = X11, noviembre = X12,
        diciembre = X13, promedio = X14
      ) %>% select(-promedio)
    
    
    tabla_tipo_cambio<-transformacion_table_tipo_cambio(tabla_tipo_cambio)
    
    tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)
  }
  
  else if (df_urls[i,1] == "tipcamb1994.pdf") {
    
    
    # url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/9401/tipcamb1994.pdf"
    
    url <- df_urls[i,2]
    
    #Locate the areas
    # locate_areas(url)
    
    
    tabla_tipo_cambio<-extract_tables(url,guess = FALSE
                                      ,area = list(c(159.464884,4.932407,692.434328,740.549833))
    )  %>% 
      as.data.frame() %>% slice(1:(n() - 3)) %>% select(-X4) %>%
      rename(
        X3 = X2, X4 = X3
        
      ) %>% separate(X1, into = c("X1","X2"), sep = " ") %>%
      rename(
        dia = X1, enero = X2, febrero = X3, marzo = X4,
        abril = X5,mayo = X6, junio = X7, julio = X8, agosto = X9,
        septiembre = X10, octubre = X11, noviembre = X12,
        diciembre = X13
      )
    
    
    tabla_tipo_cambio<-transformacion_table_tipo_cambio(tabla_tipo_cambio)
    
    tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)
  }
  
  
  
  else if (df_urls[i,1] == "tipcamb1994.pdf") {
    
    
    
    
    #url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/9401/tipcamb1994.pdf"
    
    url<- df_urls[i,2]
    
    #Locate the areas
    # locate_areas(url)
    
    
    tabla_tipo_cambio<-extract_tables(url,guess = FALSE
                                      ,area = list(c(159.464884,4.932407,692.434328,740.549833))
    )  %>% 
      as.data.frame() %>% slice(1:(n() - 3)) %>% select(-X4) %>%
      rename(
        X3 = X2, X4 = X3
        
      ) %>% separate(X1, into = c("X1","X2"), sep = " ") %>%
      rename(
        dia = X1, enero = X2, febrero = X3, marzo = X4,
        abril = X5,mayo = X6, junio = X7, julio = X8, agosto = X9,
        septiembre = X10, octubre = X11, noviembre = X12,
        diciembre = X13
      )
    
    
    tabla_tipo_cambio<-transformacion_table_tipo_cambio(tabla_tipo_cambio)
    
    tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)
  }
  
  else if (df_urls[i,1] == "tipcamb1995.pdf") {
    
    #url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/9401/tipcamb1995.pdf"
    
    url<- df_urls[i,2]
    
    tabla_tipo_cambio<-extract_tables(url,guess = FALSE
                                      ,area = list(c(159.464884,4.932407,692.434328,740.549833))
    )  %>% 
      as.data.frame() %>% slice(1:(n() - 3)) %>% select(-X4) %>%
      rename(
        X3 = X2, X4 = X3
        
      ) %>% separate(X1, into = c("X1","X2"), sep = " ") %>%
      rename(
        dia = X1, enero = X2, febrero = X3, marzo = X4,
        abril = X5,mayo = X6, junio = X7, julio = X8, agosto = X9,
        septiembre = X10, octubre = X11, noviembre = X12,
        diciembre = X13
      )
    
    tabla_tipo_cambio<-transformacion_table_tipo_cambio(tabla_tipo_cambio)
    
    tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)
    
  }
  
  
  else if (df_urls[i,1] == "tipcamb1996.pdf") {
    
    
    
    #url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/9401/tipcamb1996.pdf"
    
    
    url<- df_urls[i,2]
    # locate_areas(url)
    
    tabla_tipo_cambio<-extract_tables(url,guess = FALSE
                                      ,area = list(c(166.20616,-23.53715,710.03533,832.54320))
    )  %>% 
      as.data.frame() %>% slice(1:(n() - 3)) %>% select(-X4) %>%
      rename(
        X3 = X2, X4 = X3
        
      ) %>% separate(X1, into = c("X1","X2"), sep = " ") %>%
      rename(
        dia = X1, enero = X2, febrero = X3, marzo = X4,
        abril = X5,mayo = X6, junio = X7, julio = X8, agosto = X9,
        septiembre = X10, octubre = X11, noviembre = X12,
        diciembre = X13
      )%>% select(-X14)
    
    tabla_tipo_cambio<-transformacion_table_tipo_cambio(tabla_tipo_cambio)
    tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)
    
  }
  
  
  else if (df_urls[i,1] == "tipcamb1997.pdf") {
  
    
    
    # url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/9401/tipcamb1997.pdf"
    
    url<- df_urls[i,2]
    #Locate the areas
    # locate_areas(url)
    
    tabla_tipo_cambio<-extract_tables(url,guess = FALSE
                                      ,area = list(c(169.26862,-8.49855,687.33928,777.74791))
    ) %>% 
      as.data.frame() %>% slice(1:(n() - 3)) %>% select(-X4) %>%
      rename(
        X3 = X2, X4 = X3
        
      ) %>% separate(X1, into = c("X1","X2"), sep = " ") %>%
      rename(
        dia = X1, enero = X2, febrero = X3, marzo = X4,
        abril = X5,mayo = X6, junio = X7, julio = X8, agosto = X9,
        septiembre = X10, octubre = X11, noviembre = X12,
        diciembre = X13
      )%>% select(-X14)
    
    tabla_tipo_cambio<-transformacion_table_tipo_cambio(tabla_tipo_cambio)
    tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)
  }
    
    else if (df_urls[i,1] == "tipcamb1998.pdf") {
      
      
      
      # url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/9401/tipcamb1998.pdf"
      
      url<- df_urls[i,2]
      
      
      #Locate the areas
      # locate_areas(url)
      
      tabla_tipo_cambio<-extract_tables(url,guess = FALSE
                                        ,area = list(c(169.2686,-55.0089,687.3393,859.6947))
      ) %>% 
        as.data.frame() %>% slice(1:(n() - 3)) %>% select(-X4) %>%
        rename(
          X3 = X2, X4 = X3
          
        ) %>% separate(X1, into = c("X1","X2"), sep = " ") %>%
        rename(
          dia = X1, enero = X2, febrero = X3, marzo = X4,
          abril = X5,mayo = X6, junio = X7, julio = X8, agosto = X9,
          septiembre = X10, octubre = X11, noviembre = X12,
          diciembre = X13
        )%>% select(-X14)
      
      
      tabla_tipo_cambio<-transformacion_table_tipo_cambio(tabla_tipo_cambio)
      tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)
      
      
    }
    
    
    else if (df_urls[i,1] == "tipcamb1999.pdf") {
      
      
      
      
      # url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/9401/tipcamb1999.pdf"
      
      url<- df_urls[i,2]
      #Locate the areas
      #locate_areas(url)
      
      tabla_tipo_cambio<-extract_tables(url,guess = FALSE, 
                                        area = list(c(162.6163,145.5430,444.0993,701.2751 ))) %>% 
        as.data.frame() %>%
        rename(
          dia = X1, enero = X2, febrero = X3, marzo = X4,
          abril = X5,mayo = X6, junio = X7, julio = X8, agosto = X9,
          septiembre = X10, octubre = X11, noviembre = X12,
          diciembre = X13, promedio = X14
        )
      
      tabla_tipo_cambio<-transformacion_table_tipo_cambio(tabla_tipo_cambio)
      tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)
    }
    
    
    else if (df_urls[i,1] == "tipcamb2000.pdf") {
      
      
      
      
      # url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/9401/tipcamb2000.pdf"
      
      
      url<- df_urls[i,2]
      
      #Locate the areas
      # locate_areas(url)
      
      
      tabla_tipo_cambio<-extract_tables(url,guess = FALSE, 
                                        area = list(c(152.0398,116.1960,475.2508,781.3760))) %>% 
        as.data.frame() %>% slice(-1) %>%
        rename(
          dia = X1, enero = X2, febrero = X3, marzo = X4,
          abril = X5,mayo = X6, junio = X7, julio = X8, agosto = X9,
          septiembre = X10, octubre = X11, noviembre = X12,
          diciembre = X13
        ) %>% select(-X14)
      
      tabla_tipo_cambio<-transformacion_table_tipo_cambio(tabla_tipo_cambio)
      tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)
      
    }
    
    
    
    else if (df_urls[i,1] == "tipcamb2001.pdf") {
      
      
      
      #url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/9401/tipcamb2001.pdf"
      
      
      url<- df_urls[i,2]
      #Locate the areas
      # locate_areas(url)
      
      
      tabla_tipo_cambio<-extract_tables(url,guess = FALSE, 
                                        area = list(c(174.19441,40.22658,647.48769,693.58632))) %>% 
        as.data.frame() %>% slice(1:(n() - 3)) %>% select(-X4) %>%
        rename(
          X3 = X2, X4 = X3
          
        ) %>% separate(X1, into = c("X1","X2"), sep = " ")  %>%
        rename(
          dia = X1, enero = X2, febrero = X3, marzo = X4,
          abril = X5,mayo = X6, junio = X7, julio = X8, agosto = X9,
          septiembre = X10, octubre = X11, noviembre = X12,
          diciembre = X13
        ) %>% select(-X14)
      
      tabla_tipo_cambio<-transformacion_table_tipo_cambio(tabla_tipo_cambio)
      
      tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)
      
    }
    
    
    else if (grepl("tipcamb2002.pdf", df_urls[i,1])) {
      
      
      
      #url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/2002/tipcamb2002.pdf"
      
      
      url<- df_urls[i,2]
      
      
      #Locate the areas
      # locate_areas(url)
      
      
      tabla_tipo_cambio<-extract_tables(url,guess = FALSE, 
                                        area = list(c(162.626687,2.575344,678.483370,742.311450))) %>% 
        as.data.frame() %>% slice(1:(n() - 3)) %>% select(-X4) %>%
        rename(
          X3 = X2, X4 = X3
          
        ) %>% separate(X1, into = c("X1","X2"), sep = " ")  %>%
        rename(
          dia = X1, enero = X2, febrero = X3, marzo = X4,
          abril = X5,mayo = X6, junio = X7, julio = X8, agosto = X9,
          septiembre = X10, octubre = X11, noviembre = X12,
          diciembre = X13
        )
      
      
      tabla_tipo_cambio<-transformacion_table_tipo_cambio(tabla_tipo_cambio)
      
      tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)
    }
    
    
    else if (grepl("tipcamb2003.pdf", df_urls[i,1])) {
      
      
      
      #url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/2003/tipcamb2003.pdf"
      
      
      url<- df_urls[i,2]
      
      #Locate the areas
      #locate_areas(url)
      
      
      tabla_tipo_cambio<-extract_tables(url,guess = FALSE,
                                        area = list(c(147.75127,-24.37506,671.93550,854.84895 ))) %>% 
        as.data.frame() %>% slice(1:(n() - 3)) %>% select(-X4) %>% rename(
          X3 = X2,X4 = X3,X6 = X5,X7 = X6,X8 = X7,X9 = X8,X10 = X9,X11 = X10,X12 = X11,X13 = X12
          ,X14 = X13,X15 = X14
        ) %>% separate(X1, into = c("X1","X2"), sep = " ") %>%
        rename(
          dia = X1, enero = X2, febrero = X3, marzo = X4,
          abril = X6,mayo = X7, junio = X8, julio = X9, agosto = X10,
          septiembre = X11, octubre = X12, noviembre = X13,
          diciembre = X14, promedio = X15
        )
      
      
      tabla_tipo_cambio<-transformacion_table_tipo_cambio(tabla_tipo_cambio)
      
      tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)
      
      
    }
    
    
    else if (grepl("tipcamb2004.pdf", df_urls[i,1])) {
      
      
      
      #url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/2004/tipcamb2004.pdf"
      
      
      url<- df_urls[i,2]
      
      
      #Locate the areas
      # locate_areas(url)
      tabla_tipo_cambio<-extract_tables(url,guess = FALSE,
                                        area = list(c(147.75127,-24.37506,671.93550,854.84895 ))) %>% 
        as.data.frame() %>% slice(1:(n() - 3)) %>% select(-X4) %>% rename(
          X3 = X2,X4 = X3,X6 = X5,X7 = X6,X8 = X7,X9 = X8,X10 = X9,X11 = X10,X12 = X11,X13 = X12
          ,X14 = X13,X15 = X14
        ) %>% separate(X1, into = c("X1","X2"), sep = " ") %>%
        rename(
          dia = X1, enero = X2, febrero = X3, marzo = X4,
          abril = X6,mayo = X7, junio = X8, julio = X9, agosto = X10,
          septiembre = X11, octubre = X12, noviembre = X13,
          diciembre = X14, promedio = X15
        )
      
      tabla_tipo_cambio<-transformacion_table_tipo_cambio(tabla_tipo_cambio)
      
      tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)
    }
    
    
    
    else if (grepl("tipcamb2005.pdf", df_urls[i,1])) {
      
      
      
      #url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/2005/tipcamb2005.pdf"
      
      url<- df_urls[i,2]
      
      
      #Locate the areas
      # locate_areas(url)
      tabla_tipo_cambio<-extract_tables(url,guess = FALSE,
                                        area = list(c(130.75127,-24.37506,671.93550,854.84895 ))) %>% 
        as.data.frame() %>% slice(1:(n() - 3)) %>% select(-X4) %>% rename(
          X3 = X2,X4 = X3,X6 = X5,X7 = X6,X8 = X7,X9 = X8,X10 = X9,X11 = X10,X12 = X11,X13 = X12
          ,X14 = X13,X15 = X14
        ) %>% separate(X1, into = c("X1","X2"), sep = " ") %>%
        rename(
          dia = X1, enero = X2, febrero = X3, marzo = X4,
          abril = X6,mayo = X7, junio = X8, julio = X9, agosto = X10,
          septiembre = X11, octubre = X12, noviembre = X13,
          diciembre = X14, promedio = X15
        )
      
      
      tabla_tipo_cambio<-transformacion_table_tipo_cambio(tabla_tipo_cambio)
      
      tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)
      
    }
    
    else if (grepl("tipcamb2006.pdf", df_urls[i,1])) {
      
      
      
      #url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/2006/tipcamb2006.pdf"
      
      url<- df_urls[i,2]
      
      
      #Locate the areas
      # locate_areas(url)
      tabla_tipo_cambio<-extract_tables(url,guess = FALSE,
                                        area = list(c(147.75127,-24.37506,671.93550,854.84895 ))) %>% 
        as.data.frame() %>% slice(1:(n() - 3)) %>% select(-X4) %>% rename(
          X3 = X2,X4 = X3,X6 = X5,X7 = X6,X8 = X7,X9 = X8,X10 = X9,X11 = X10,X12 = X11,X13 = X12
          ,X14 = X13,X15 = X14
        ) %>% separate(X1, into = c("X1","X2"), sep = " ") %>%
        rename(
          dia = X1, enero = X2, febrero = X3, marzo = X4,
          abril = X6,mayo = X7, junio = X8, julio = X9, agosto = X10,
          septiembre = X11, octubre = X12, noviembre = X13,
          diciembre = X14, promedio = X15
        ) %>% slice(-1)
      
      tabla_tipo_cambio<-transformacion_table_tipo_cambio(tabla_tipo_cambio)
      
      tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)
    }
    
    
    
    else if (grepl("tipcamb2007.pdf", df_urls[i,1])) {
      
      
      #url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/2007/tipcamb2007.pdf"
      
      url<- df_urls[i,2]
      
      #Locate the areas
      # locate_areas(url)
      tabla_tipo_cambio<-extract_tables(url,guess = FALSE,
                                        area = list(c(147.75127,-24.37506,671.93550,854.84895 ))) %>% 
        as.data.frame() %>% slice(1:(n() - 3)) %>% select(-X4) %>% rename(
          X3 = X2,X4 = X3,X6 = X5,X7 = X6,X8 = X7,X9 = X8,X10 = X9,X11 = X10,X12 = X11,X13 = X12
          ,X14 = X13,X15 = X14
        ) %>% separate(X1, into = c("X1","X2"), sep = " ") %>%
        rename(
          dia = X1, enero = X2, febrero = X3, marzo = X4,
          abril = X6,mayo = X7, junio = X8, julio = X9, agosto = X10,
          septiembre = X11, octubre = X12, noviembre = X13,
          diciembre = X14, promedio = X15
        ) %>% slice(-1)
      
      
      tabla_tipo_cambio<-transformacion_table_tipo_cambio(tabla_tipo_cambio)
      
      tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)
    }
    
    
    
    else if (grepl("tipocambio2008.pdf", df_urls[i,1])) {
      
      
      #url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/2008/tipocambio2008.pdf"
      
      url<- df_urls[i,2]
      
      #Locate the areas
      # locate_areas(url)
      tabla_tipo_cambio<-extract_tables(url,guess = FALSE,
                                        area = list(c(147.75127,-24.37506,671.93550,854.84895 ))) %>% 
        as.data.frame() %>% slice(1:(n() - 3)) %>% select(-X4) %>% rename(
          X3 = X2,X4 = X3,X6 = X5,X7 = X6,X8 = X7,X9 = X8,X10 = X9,X11 = X10,X12 = X11,X13 = X12
          ,X14 = X13,X15 = X14
        ) %>% separate(X1, into = c("X1","X2"), sep = " ") %>%
        rename(
          dia = X1, enero = X2, febrero = X3, marzo = X4,
          abril = X6,mayo = X7, junio = X8, julio = X9, agosto = X10,
          septiembre = X11, octubre = X12, noviembre = X13,
          diciembre = X14, promedio = X15
        ) %>% slice(-1)
      
      
      tabla_tipo_cambio<-transformacion_table_tipo_cambio(tabla_tipo_cambio)
      
      
      tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)
      
    }
    
    
    else if (grepl("tipocambio2009.pdf", df_urls[i,1])) {
      
      
      #url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/2009/tipocambio2009.pdf"
      
      
      url<- df_urls[i,2]
      #Locate the areas
      # locate_areas(url)
      tabla_tipo_cambio<-extract_tables(url,guess = FALSE,
                                        area = list(c(147.75127,-24.37506,671.93550,854.84895 ))) %>% 
        as.data.frame() %>% slice(1:(n() - 3)) %>% select(-X5,-X1) %>% rename(
          X5 = X4
        ) %>% separate(X2, into = c("X1","X2"), sep = " ") %>%
        rename(
          dia = X1, enero = X2, febrero = X3, marzo = X5,
          abril = X6,mayo = X7, junio = X8, julio = X9, agosto = X10,
          septiembre = X11, octubre = X12, noviembre = X13,
          diciembre = X14, promedio = X15
        ) %>% slice(-1)
      
      
      tabla_tipo_cambio<-transformacion_table_tipo_cambio(tabla_tipo_cambio)
      tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)
      
    }
    
    
    else if (grepl("tipocambio2010.pdf", df_urls[i,1])) {
      
      
      
      #url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/2010/tipocambio2010.pdf"
      
      
      url<- df_urls[i,2]
      #Locate the areas
      # locate_areas(url)
      tabla_tipo_cambio<-extract_tables(url,guess = FALSE,
                                        area = list(c(178.70089,21.19906,555.07701,805.02939))) %>% 
        as.data.frame() %>% slice(-1) %>%
        rename(
          dia = X1, enero = X2, febrero = X3, marzo = X4,
          abril = X5,mayo = X6, junio = X7, julio = X8, agosto = X9,
          septiembre = X10, octubre = X11, noviembre = X12,
          diciembre = X13, promedio = X14
        )
      
      tabla_tipo_cambio<-transformacion_table_tipo_cambio(tabla_tipo_cambio)
      tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)
    }
    
    
    
    else if (grepl("tipocambio2011.pdf", df_urls[i,1])) {
      
      
      
      #url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/2011/tipocambio2011.pdf"
      
      
      url<- df_urls[i,2]
      #Locate the areas
      # locate_areas(url)
      
      
      tabla_tipo_cambio<-extract_tables(url,guess = FALSE,
                                        area = list(c(251.46274,51.15055,791.84666,1173.34439))) %>% 
        as.data.frame() %>% 
        rename(
          dia = X1, enero = X2, febrero = X3, marzo = X4,
          abril = X5,mayo = X6, junio = X7, julio = X8, agosto = X9,
          septiembre = X10, octubre = X11, noviembre = X12,
          diciembre = X13, promedio = X14
        ) %>% slice(-1)
      
      tabla_tipo_cambio<-transformacion_table_tipo_cambio(tabla_tipo_cambio)
      tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)
    }
    
    
    else if (grepl("tipocambio2012.pdf", df_urls[i,1])) {
      
      
      #url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/2012/tipocambio2012.pdf"
      
      url<- df_urls[i,2]
      
      #Locate the areas
      #  locate_areas(url)
      
      
      tabla_tipo_cambio<-extract_tables(url,guess = FALSE,
                                        area = list(c(161.69144,27.47679,528.27432,788.40520))) %>% 
        as.data.frame() %>% 
        rename(
          dia = X1, enero = X2, febrero = X3, marzo = X4,
          abril = X5,mayo = X6, junio = X7, julio = X8, agosto = X9,
          septiembre = X10, octubre = X11, noviembre = X12,
          diciembre = X13, promedio = X14
        ) %>% slice(-1)
      
      
      tabla_tipo_cambio<-transformacion_table_tipo_cambio(tabla_tipo_cambio)
      tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)
      
    }
    
    
    else if (grepl("tipocambio2013.pdf", df_urls[i,1])) {
      
      
      
      #url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/2013/tipocambio2013.pdf"
      
      url<- df_urls[i,2]
      
      #Locate the areas
      # locate_areas(url)
      
      
      tabla_tipo_cambio<-extract_tables(url,guess = FALSE,
                                        area = list(c(161.69144,27.47679,528.27432,788.40520))) %>% 
        as.data.frame() %>% 
        rename(
          dia = X1, enero = X2, febrero = X3, marzo = X4,
          abril = X5,mayo = X6, junio = X7, julio = X8, agosto = X9,
          septiembre = X10, octubre = X11, noviembre = X12,
          diciembre = X13, promedio = X14
        ) %>% slice(-1)
      
      tabla_tipo_cambio<-transformacion_table_tipo_cambio(tabla_tipo_cambio)
      tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)
    }
    
    
    else if (grepl("tipocambio2014.pdf", df_urls[i,1])) {
      
      
      
      
      #url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/2014/tipocambio2014.pdf"
      
      url<- df_urls[i,2]
      
      #Locate the areas
      # locate_areas(url)
      
      
      tabla_tipo_cambio<-extract_tables(url,guess = FALSE,
                                        area = list(c(161.69144,27.47679,528.27432,788.40520))) %>% 
        as.data.frame() %>% 
        rename(
          dia = X1, enero = X2, febrero = X3, marzo = X4,
          abril = X5,mayo = X6, junio = X7, julio = X8, agosto = X9,
          septiembre = X10, octubre = X11, noviembre = X12,
          diciembre = X13, promedio = X14
        ) %>% slice(-1)
      
      tabla_tipo_cambio<-transformacion_table_tipo_cambio(tabla_tipo_cambio)
      tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)
    }
    
    
    else if (grepl("tipocambio2015.pdf", df_urls[i,1])) {
      
      
      
      # url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/2015/tipocambio2015.pdf"
      
      url<- df_urls[i,2]
      
      
      #Locate the areas
      # locate_areas(url)
      
      
      tabla_tipo_cambio<-extract_tables(url,guess = FALSE,
                                        area = list(c(148.79787,25.59011,503.10675,767.40607))) %>% 
        as.data.frame() %>% 
        rename(
          dia = X1, enero = X2, febrero = X3, marzo = X4,
          abril = X5,mayo = X6, junio = X7, julio = X8, agosto = X9,
          septiembre = X10, octubre = X11, noviembre = X12,
          diciembre = X13, promedio = X14
        ) %>% slice(-1)
      
      tabla_tipo_cambio<-transformacion_table_tipo_cambio(tabla_tipo_cambio)
      tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)
    }
    
    
    else if (grepl("tipocambio2016.pdf", df_urls[i,1])) {
      
      
      
      url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/2016/tipocambio2016.pdf"
      
      url<- df_urls[i,2]
      
      #Locate the areas
      # locate_areas(url)
      
      
      tabla_tipo_cambio<-extract_tables(url,guess = FALSE,
                                        area = list(c(291.53573,35.85539,828.96434,1224.80605))) %>% 
        as.data.frame() %>% 
        rename(
          dia = X1, enero = X2, febrero = X3, marzo = X4,
          abril = X5,mayo = X6, junio = X7, julio = X8, agosto = X9,
          septiembre = X10, octubre = X11, noviembre = X12,
          diciembre = X13, promedio = X14
        ) %>% slice(-1)
      
      tabla_tipo_cambio<-transformacion_table_tipo_cambio(tabla_tipo_cambio)
      tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)
      
    }
    
    else if (grepl("tipocambio2017.pdf", df_urls[i,1])) {
      
      
      
      
      # url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/2017/tipocambio2017.pdf"
      
      
      url<- df_urls[i,2]
      
      
      #Locate the areas
      # locate_areas(url)
      
      
      tabla_tipo_cambio<-extract_tables(url,guess = FALSE,
                                        area = list(c(234.22156,23.07445,770.84462,1129.53188))) %>% 
        as.data.frame() %>% 
        rename(
          dia = X1, enero = X2, febrero = X3, marzo = X4,
          abril = X5,mayo = X6, junio = X7, julio = X8, agosto = X9,
          septiembre = X10, octubre = X11, noviembre = X12,
          diciembre = X13, promedio = X14
        ) %>% slice(-1)
      
      tabla_tipo_cambio<-transformacion_table_tipo_cambio(tabla_tipo_cambio)
      tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)
    }
    
    
    else if (grepl("2018.pdf", df_urls[i,1])) {
      
      
      
      # url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/1823/2018.pdf"
      
      url<- df_urls[i,2]
      
      
      #Locate the areas
      # locate_areas(url)
      
      
      tabla_tipo_cambio<-extract_tables(url,guess = FALSE
                                        ,area = list(c(170.74287,13.88878,539.58860,892.57993))
      ) %>% 
        as.data.frame() %>% 
        rename(
          dia = X1, enero = X2, febrero = X3, marzo = X4,
          abril = X5,mayo = X6, junio = X7, julio = X8, agosto = X9,
          septiembre = X10, octubre = X11, noviembre = X12,
          diciembre = X13, promedio = X14
        ) %>% slice(-1)
      
      tabla_tipo_cambio<-transformacion_table_tipo_cambio(tabla_tipo_cambio)
      tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)
      
    }
    
    else if (grepl("2019.pdf", df_urls[i,1])) {
      
      
      
      #url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/1823/2019.pdf"
      
      url<- df_urls[i,2]
      
      #Locate the areas
      # locate_areas(url)
      
      
      tabla_tipo_cambio<-extract_tables(url,guess = FALSE
                                        ,area = list(c(170.74287,13.88878,539.58860,892.57993))
      ) %>% 
        as.data.frame() %>% 
        rename(
          dia = X1, enero = X2, febrero = X3, marzo = X4,
          abril = X5,mayo = X6, junio = X7, julio = X8, agosto = X9,
          septiembre = X10, octubre = X11, noviembre = X12,
          diciembre = X13, promedio = X14
        ) %>% slice(-1)
      
      
      tabla_tipo_cambio<-transformacion_table_tipo_cambio(tabla_tipo_cambio)
      tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)
      
    }
    
    else if (grepl("2020.pdf", df_urls[i,1])) {
      
      
      
      # url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/1823/2020.pdf"
      
      url<- df_urls[i,2]
      
      
      #Locate the areas
      # locate_areas(url)
      
      
      tabla_tipo_cambio<-extract_tables(url,guess = FALSE
                                        ,area = list(c(170.74287,13.88878,539.58860,892.57993))
      ) %>% 
        as.data.frame() %>% 
        rename(
          dia = X1, enero = X2, febrero = X3, marzo = X4,
          abril = X5,mayo = X6, junio = X7, julio = X8, agosto = X9,
          septiembre = X10, octubre = X11, noviembre = X12,
          diciembre = X13, promedio = X14
        ) %>% slice(-1)
      
      tabla_tipo_cambio<-transformacion_table_tipo_cambio(tabla_tipo_cambio)
      tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)
    }
    
    else if (grepl("2021.pdf", df_urls[i,1])) {
      
      # url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/1823/2021.pdf"
      
      
      url<- df_urls[i,2]
      
      
      #Locate the areas
      # locate_areas(url)
      
      
      tabla_tipo_cambio<-extract_tables(url,guess = FALSE
                                        ,area = list(c(170.74287,13.88878,539.58860,892.57993))
      ) %>% 
        as.data.frame() %>% 
        rename(
          dia = X1, enero = X2, febrero = X3, marzo = X4,
          abril = X5,mayo = X6, junio = X7, julio = X8, agosto = X9,
          septiembre = X10, octubre = X11, noviembre = X12,
          diciembre = X13, promedio = X14
        ) %>% slice(-1)
      
      tabla_tipo_cambio<-transformacion_table_tipo_cambio(tabla_tipo_cambio)
      tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)
      
      
    }
    
    
    else if (grepl("2022.pdf", df_urls[i,1])) {
      
      
      
      #url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/1823/2022.pdf"
      
      url<- df_urls[i,2]
      
      
      #Locate the areas
      # locate_areas(url)
      
      
      tabla_tipo_cambio<-extract_tables(url,guess = FALSE
                                        ,area = list(c(170.74287,13.88878,539.58860,892.57993))
      ) %>% 
        as.data.frame() %>% 
        rename(
          dia = X1, enero = X2, febrero = X3, marzo = X4,
          abril = X5,mayo = X6, junio = X7, julio = X8, agosto = X9,
          septiembre = X10, octubre = X11, noviembre = X12,
          diciembre = X13, promedio = X14
        ) %>% slice(-1)
      
      
      tabla_tipo_cambio<-transformacion_table_tipo_cambio(tabla_tipo_cambio)
      tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)
      
    }
    
    
    else if (grepl("2023.pdf", df_urls[i,1])) {
      
      
      
      #url <- "https://www.bcn.gob.ni/IRR/tipo_cambio_mensual/cambio_historico/1823/2023.pdf"
      
      url<- df_urls[i,2]
      
      #Locate the areas
      # locate_areas(url)
      
      
      tabla_tipo_cambio<-extract_tables(url,guess = FALSE
                                        ,area = list(c(170.74287,13.88878,539.58860,892.57993))
      ) %>% 
        as.data.frame() %>% 
        rename(
          dia = X1, enero = X2, febrero = X3, marzo = X4,
          abril = X5,mayo = X6, junio = X7, julio = X8, agosto = X9,
          septiembre = X10, octubre = X11, noviembre = X12,
          diciembre = X13, promedio = X14
        ) %>% slice(-1)
      
      tabla_tipo_cambio<-transformacion_table_tipo_cambio(tabla_tipo_cambio)
      
      tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)
    }
    
  
  else {
  
  tabla_tipo_cambio<-data.frame()
  
  print(paste("iteracion numero: ", i))
  
  url <- df_urls[i,2] # df_urls[i,2] urls
  
  
  tabla_tipo_cambio<-extract_tables(url,guess = TRUE) %>% 
    as.data.frame() %>%
    rename(
      dia = X1, enero = X2, febrero = X3, marzo = X4,
      abril = X5,mayo = X6, junio = X7, julio = X8, agosto = X9,
      septiembre = X10, octubre = X11, noviembre = X12,
      diciembre = X13, promedio = X14
    )
  
  
  # remover la columna promedio
  tabla_tipo_cambio<-tabla_tipo_cambio[,-which(names(tabla_tipo_cambio) == "promedio")]
  
  tabla_tipo_cambio <- tabla_tipo_cambio[tabla_tipo_cambio$dia != "Promedio", ]
  
  
  
  # crear la tabla inversa 
  tabla_tipo_cambio<-pivot_longer(tabla_tipo_cambio, cols =-dia
                                  ,names_to = "mes",values_to = "valor")
  
  
  # capturar el ano del 
  ano_documento<- gsub("\\D+", "", df_urls[i,1] ) #df_urls[i,1] para cada nombre de archivo
  
  
  
  tabla_tipo_cambio<- tabla_tipo_cambio %>% 
    left_join(tabla_meses, by=c("mes"="NombreMes")) %>%
    mutate(ano = ano_documento) %>% select(-c(mes))
  
  tabla_tipo_cambio$NumeroMes<-as.character(tabla_tipo_cambio$NumeroMes)
  
  tabla_tipo_cambio$fecha <- as.Date(paste(tabla_tipo_cambio$ano,
                                           tabla_tipo_cambio$NumeroMes,
                                           tabla_tipo_cambio$dia,sep="-"))
  
  
  tabla_tipo_cambio <- tabla_tipo_cambio %>% select(-c(dia,NumeroMes,ano)) %>% 
    select(fecha,valor) %>% rename(TipoCambio  = valor) %>% arrange(fecha)
  
  tabla_tipo_cambio$TipoCambio<-as.numeric(tabla_tipo_cambio$TipoCambio)
  #str(tabla_tipo_cambio)
  
  
  tabla_tipo_cambio_nicaragua <- union(tabla_tipo_cambio_nicaragua, tabla_tipo_cambio)

}

  
}

# quitar los NAs
tabla_tipo_cambio_nicaragua<-na.omit(tabla_tipo_cambio_nicaragua)

tabla_tipo_cambio_nicaragua <- tabla_tipo_cambio_nicaragua[order(tabla_tipo_cambio_nicaragua$fecha), ]


# Prueba
plot(tabla_tipo_cambio_nicaragua[tabla_tipo_cambio_nicaragua$fecha > as.Date("1992-01-01"), ])

write.csv(tabla_tipo_cambio_nicaragua, "path/TipoCambioNicaraguaHistorico.csv"
          , row.names=FALSE)
 

unique(tabla_tipo_cambio_nicaragua$fecha)
remote.driver$server$stop()




 # 1993 
# 
# 
# 
# 
# # remover la columna promedio
# tabla_tipo_cambio<-tabla_tipo_cambio[,-which(names(tabla_tipo_cambio) == "promedio")]
# 
# # crear la tabla inversa 
# tabla_tipo_cambio<-pivot_longer(tabla_tipo_cambio, cols =-dia
#                                 ,names_to = "mes",values_to = "valor")
# 
# 
# tabla_tipo_cambio <- tabla_tipo_cambio %>%
#   mutate(ano = gsub("\\D", "",sub(".*/([^/]+)$", "\\1", url))) %>% 
#   left_join(tabla_meses, by=c("mes"="NombreMes"))
# 
# tabla_tipo_cambio$fechastring <- paste(tabla_tipo_cambio$dia
#                                        ,as.character(tabla_tipo_cambio$NumeroMes)
#                                        ,tabla_tipo_cambio$ano,sep="-")
# 
# tabla_tipo_cambio$fecha <- as.Date(tabla_tipo_cambio$fechastring, format = "%d-%m-%Y")
# 
# 
# tabla_tipo_cambio<- tabla_tipo_cambio %>% 
#   select(-c(dia,mes,ano,NumeroMes,fechastring)) %>%
#   select(fecha,valor) %>%  rename(TipoCambio = valor)
# 
# 
# tabla_tipo_cambio$TipoCambio<-as.numeric(tabla_tipo_cambio$TipoCambio)
# 
# tabla_tipo_cambio<-na.omit(tabla_tipo_cambio)
