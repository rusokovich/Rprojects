library(PerformanceAnalytics)
library(xts)
library(ggplot2)

## Correlacion Petroleo Anual
rm(list=ls())

# cargar los datos 

setwd('/Users/fkkarpuk/Downloads/')

# subir los datos, 

fuente<-read.csv('EleccionesYTipoCambio.csv')

# Transformaciones

fuente$fecha<-as.Date(fuente$fecha)


xts.compra<-xts(fuente[,c("tipo_cambio_compra")],
                         order.by = fuente$fecha) 

ts.compra.diff<- diff(log(xts.compra))
plot(xts.compra)
plot(ts.compra.diff)

xts.compra.retornos <- CalculateReturns(xts.compra, method = "log")
plot(xts.compra.retornos)


class(xts.compra.retornos)


df.varianza.retornos <- as.data.frame(xts.compra.retornos)
df.varianza.retornos$fecha <- rownames(df.varianza.retornos)


class(df.varianza.retornos$fecha)

df.varianza.retornos$fecha<-sub("X", "", df.varianza.retornos$fecha)
df.varianza.retornos$fecha<-as.Date(df.varianza.retornos$fecha,"%Y.%m.%d")


df.varianza.retornos$fecha<-as.Date(df.varianza.retornos$fecha)
df.varianza.retornos<-merge(df.varianza.retornos,fuente,by="fecha")

# serie de tiempo completa 

xts.compra.historico<-xts(fuente[,c("tipo_cambio_compra")],
                         order.by = fuente$fecha) 


ts.compra.diff<- CalculateReturns(xts.compra.historico, method = "log")
plot(ts.compra.diff, main = "Variacion Historica", ylab = "Varianza")

# datos historicos varianza
data.frame.retornos.historicos <- as.data.frame(ts.compra.diff)
data.frame.retornos.historicos$fecha <- rownames(data.frame.retornos.historicos)

data.frame.retornos.historicos$fecha<-sub("X", "", data.frame.retornos.historicos$fecha)
data.frame.retornos.historicos$fecha<-as.Date(data.frame.retornos.historicos$fecha,"%Y.%m.%d")

data.frame.retornos.historicos<-merge(data.frame.retornos.historicos,fuente,by="fecha")

colnames(data.frame.retornos.historicos)<-c("fecha","varianza"
                                            ,"tipo_cambio_compra"
                                            ,"tipo_cambio_venta","dias_para_eleccion")

data.frame.retornos.historicos$flag<-ifelse(data.frame.retornos.historicos$dias_para_eleccion>180
                                            &
                                            data.frame.retornos.historicos$dias_para_eleccion<1300,
                                            0,1)
str(data.frame.retornos.historicos)

ggplot(data.frame.retornos.historicos, aes(x = fecha, y = varianza, color = factor(flag))) +
  geom_line(aes(group = 1)) +
  scale_color_manual(values = c("1" = "red", "0" = "blue"), 
                     labels = c("Período no Electoral", "Período Electoral"),
                     guide = guide_legend(title = "Categoría")) +
  labs(title = "Varianza de las Elecciones 1953 - 2023")+ 
  theme(plot.title = element_text(hjust = 0.5))



# serie de tiempo 80-90


fecha.filtro.inicio <- as.Date("1980-01-01")
fecha.filtro.final <- as.Date("1990-01-01")
datos.filtrados <- fuente[fuente$fecha >= fecha.filtro.inicio 
                          & fuente$fecha <= fecha.filtro.final, ]

xts.compra.filtrado<-xts(datos.filtrados[,c("tipo_cambio_compra")],
                         order.by = datos.filtrados$fecha) 


ts.compra.diff.filtrado<- CalculateReturns(xts.compra.filtrado, method = "log")
plot(ts.compra.diff.filtrado, main = "Variacion Historica", ylab = "Varianza")

# datos parciales varianza
data.frame.retornos.filtrados.80 <- as.data.frame(ts.compra.diff.filtrado)
data.frame.retornos.filtrados.80$fecha <- rownames(data.frame.retornos.filtrados.80)
data.frame.retornos.filtrados.80$fecha<-sub("X", "", data.frame.retornos.filtrados.80$fecha)
data.frame.retornos.filtrados.80$fecha<-as.Date(data.frame.retornos.filtrados.80$fecha,"%Y.%m.%d")


data.frame.retornos.filtrados.80$fecha<-as.Date(data.frame.retornos.filtrados.80$fecha)

data.frame.retornos.filtrados.80<-merge(data.frame.retornos.filtrados.80,fuente,by="fecha")

colnames(data.frame.retornos.filtrados.80)<-c("fecha","varianza"
                                            ,"tipo_cambio_compra"
                                            ,"tipo_cambio_venta","dias_para_eleccion")

# ultimos 6 meses de gobierno y primeros 200 dias del nuevo
data.frame.retornos.filtrados.80$flag<-ifelse(data.frame.retornos.filtrados.80$dias_para_eleccion>180
                                            &
                                              data.frame.retornos.filtrados.80$dias_para_eleccion<1300,
                                            0,1)
data.frame.retornos.filtrados.80$flag<-as.factor(data.frame.retornos.filtrados.80$flag)



ggplot(data.frame.retornos.filtrados.80, aes(x = fecha, y = varianza, color = flag)) +
  geom_line(aes(group = 1)) +
  scale_color_manual(values = c("1" = "red", "0" = "blue"), 
                     labels = c("Período no Electoral", "Período Electoral"),
                     guide = guide_legend(title = "Categoría")) +
  labs(title = "Varianza de las Elecciones 1980 - 1990")+ 
  theme(plot.title = element_text(hjust = 0.5))

varianza.elecciones.tipocambio.mediana.80 <- aggregate(varianza ~ dias_para_eleccion
                             , data=data.frame.retornos.filtrados.80
                             , FUN = median)



ggplot(varianza.elecciones.tipocambio.mediana.80, aes(x = dias_para_eleccion, y = varianza)) +
  geom_line(aes(group = 1)) +  labs(title = "Volatilidad media por días para elección 1980 - 1990")+ 
  theme(plot.title = element_text(hjust = 0.5))

varianza.elecciones.tipocambio.promedio.80 <- aggregate(varianza ~ dias_para_eleccion
                             , data=data.frame.retornos.filtrados.80
                             , FUN = mean)

ggplot(varianza.elecciones.tipocambio.promedio.80, aes(x = dias_para_eleccion, y = varianza)) +
  geom_line(aes(group = 1)) +  labs(title = "Volatilidad promedio por días para elección 1980 - 1990")+ 
  theme(plot.title = element_text(hjust = 0.5))


print(varianza.elecciones.tipocambio.mediana.80)

# serie de tiempo 90-00


fecha.filtro.inicio <- as.Date("1990-01-01")
fecha.filtro.final <- as.Date("2000-01-01")
datos.filtrados <- fuente[fuente$fecha >= fecha.filtro.inicio 
                          & fuente$fecha <= fecha.filtro.final, ]

xts.compra.filtrado<-xts(datos.filtrados[,c("tipo_cambio_compra")],
                         order.by = datos.filtrados$fecha) 


ts.compra.diff.filtrado<- CalculateReturns(xts.compra.filtrado, method = "log")
plot(ts.compra.diff.filtrado, main = "Variacion Historica", ylab = "Varianza")

# datos parciales varianza
data.frame.retornos.filtrados.90 <- as.data.frame(ts.compra.diff.filtrado)
data.frame.retornos.filtrados.90$fecha <- rownames(data.frame.retornos.filtrados.90)

data.frame.retornos.filtrados.90$fecha<-as.Date(data.frame.retornos.filtrados.90$fecha)

data.frame.retornos.filtrados.90<-merge(data.frame.retornos.filtrados.90,fuente,by="fecha")

colnames(data.frame.retornos.filtrados.90)<-c("fecha","varianza"
                                              ,"tipo_cambio_compra"
                                              ,"tipo_cambio_venta","dias_para_eleccion")

# ultimos 6 meses de gobierno y primeros 200 dias del nuevo
data.frame.retornos.filtrados.90$flag<-ifelse(data.frame.retornos.filtrados.90$dias_para_eleccion>180
                                              &
                                                data.frame.retornos.filtrados.90$dias_para_eleccion<1300,
                                              0,1)
data.frame.retornos.filtrados.90$flag<-as.factor(data.frame.retornos.filtrados.90$flag)



ggplot(data.frame.retornos.filtrados.90, aes(x = fecha, y = varianza, color = flag)) +
  geom_line(aes(group = 1)) +
  scale_color_manual(values = c("1" = "red", "0" = "blue"), 
                     labels = c("Período no Electoral", "Período Electoral"),
                     guide = guide_legend(title = "Categoría")) +
  labs(title = "Varianza de las Elecciones 1990 - 2000")+ 
  theme(plot.title = element_text(hjust = 0.5))

varianza.elecciones.tipocambio.mediana.90 <- aggregate(varianza ~ dias_para_eleccion
                                                       , data=data.frame.retornos.filtrados.90
                                                       , FUN = median)



ggplot(varianza.elecciones.tipocambio.mediana.90, aes(x = dias_para_eleccion, y = varianza)) +
  geom_line(aes(group = 1)) +  labs(title = "Volatilidad media por días para elección 1990 - 2000")+ 
  theme(plot.title = element_text(hjust = 0.5))

varianza.elecciones.tipocambio.promedio.90 <- aggregate(varianza ~ dias_para_eleccion
                                                        , data=data.frame.retornos.filtrados.90
                                                        , FUN = mean)

ggplot(varianza.elecciones.tipocambio.promedio.90, aes(x = dias_para_eleccion, y = varianza)) +
  geom_line(aes(group = 1)) +  labs(title = "Volatilidad promedio por días para elección 1990 - 2000")+ 
  theme(plot.title = element_text(hjust = 0.5))


print(varianza.elecciones.tipocambio.mediana.90)


# serie de tiempo 00-10
fecha.filtro.inicio <- as.Date("2000-01-01")
fecha.filtro.final <- as.Date("2010-01-01")
datos.filtrados <- fuente[fuente$fecha >= fecha.filtro.inicio 
                          & fuente$fecha <= fecha.filtro.final, ]

xts.compra.filtrado<-xts(datos.filtrados[,c("tipo_cambio_compra")],
                         order.by = datos.filtrados$fecha) 


ts.compra.diff.filtrado<- CalculateReturns(xts.compra.filtrado, method = "log")
plot(ts.compra.diff.filtrado, main = "Variacion Historica", ylab = "Varianza")

# datos parciales varianza
data.frame.retornos.filtrados.00 <- as.data.frame(ts.compra.diff.filtrado)
data.frame.retornos.filtrados.00$fecha <- rownames(data.frame.retornos.filtrados.00)

data.frame.retornos.filtrados.00$fecha<-as.Date(data.frame.retornos.filtrados.00$fecha)

data.frame.retornos.filtrados.00<-merge(data.frame.retornos.filtrados.00,fuente,by="fecha")

colnames(data.frame.retornos.filtrados.00)<-c("fecha","varianza"
                                              ,"tipo_cambio_compra"
                                              ,"tipo_cambio_venta","dias_para_eleccion")

# ultimos 6 meses de gobierno y primeros 200 dias del nuevo
data.frame.retornos.filtrados.00$flag<-ifelse(data.frame.retornos.filtrados.00$dias_para_eleccion>180
                                              &
                                                data.frame.retornos.filtrados.00$dias_para_eleccion<1300,
                                              0,1)
data.frame.retornos.filtrados.00$flag<-as.factor(data.frame.retornos.filtrados.00$flag)



ggplot(data.frame.retornos.filtrados.00, aes(x = fecha, y = varianza, color = flag)) +
  geom_line(aes(group = 1)) +
  scale_color_manual(values = c("1" = "red", "0" = "blue"), 
                     labels = c("Período no Electoral", "Período Electoral"),
                     guide = guide_legend(title = "Categoría")) +
  labs(title = "Varianza de las Elecciones 2000 - 2010")+ 
  theme(plot.title = element_text(hjust = 0.5))

varianza.elecciones.tipocambio.mediana.00 <- aggregate(varianza ~ dias_para_eleccion
                                                       , data=data.frame.retornos.filtrados.00
                                                       , FUN = median)



ggplot(varianza.elecciones.tipocambio.mediana.00, aes(x = dias_para_eleccion, y = varianza)) +
  geom_line(aes(group = 1)) +  labs(title = "Volatilidad media por días para elección 2000 - 2010")+ 
  theme(plot.title = element_text(hjust = 0.5))

varianza.elecciones.tipocambio.promedio.00 <- aggregate(varianza ~ dias_para_eleccion
                                                        , data=data.frame.retornos.filtrados.00
                                                        , FUN = mean)

ggplot(varianza.elecciones.tipocambio.promedio.00, aes(x = dias_para_eleccion, y = varianza)) +
  geom_line(aes(group = 1)) +  labs(title = "Volatilidad promedio por días para elección 2000 - 2010")+ 
  theme(plot.title = element_text(hjust = 0.5))


print(varianza.elecciones.tipocambio.mediana.00)




# serie de tiempo 10-xx


fecha.filtro.inicio <- as.Date("2010-01-01")
fecha.filtro.final <- as.Date("2024-01-01")
datos.filtrados <- fuente[fuente$fecha >= fecha.filtro.inicio 
                          & fuente$fecha <= fecha.filtro.final, ]

xts.compra.filtrado<-xts(datos.filtrados[,c("tipo_cambio_compra")],
                         order.by = datos.filtrados$fecha) 


ts.compra.diff.filtrado<- CalculateReturns(xts.compra.filtrado, method = "log")
plot(ts.compra.diff.filtrado, main = "Variacion Historica", ylab = "Varianza")

# datos parciales varianza
data.frame.retornos.filtrados.10 <- as.data.frame(ts.compra.diff.filtrado)
data.frame.retornos.filtrados.10$fecha <- rownames(data.frame.retornos.filtrados.10)

data.frame.retornos.filtrados.10$fecha<-as.Date(data.frame.retornos.filtrados.10$fecha)

data.frame.retornos.filtrados.10<-merge(data.frame.retornos.filtrados.10,fuente,by="fecha")

colnames(data.frame.retornos.filtrados.10)<-c("fecha","varianza"
                                              ,"tipo_cambio_compra"
                                              ,"tipo_cambio_venta","dias_para_eleccion")

# ultimos 6 meses de gobierno y primeros 200 dias del nuevo
data.frame.retornos.filtrados.10$flag<-ifelse(data.frame.retornos.filtrados.10$dias_para_eleccion>180
                                              &
                                                data.frame.retornos.filtrados.10$dias_para_eleccion<1300,
                                              0,1)
data.frame.retornos.filtrados.10$flag<-as.factor(data.frame.retornos.filtrados.10$flag)



ggplot(data.frame.retornos.filtrados.10, aes(x = fecha, y = varianza, color = flag)) +
  geom_line(aes(group = 1)) +
  scale_color_manual(values = c("1" = "red", "0" = "blue"), 
                     labels = c("Período no Electoral", "Período Electoral"),
                     guide = guide_legend(title = "Categoría")) +
  labs(title = "Varianza de las Elecciones 2010 - 2023")+ 
  theme(plot.title = element_text(hjust = 0.5))

varianza.elecciones.tipocambio.mediana.10 <- aggregate(varianza ~ dias_para_eleccion
                                                       , data=data.frame.retornos.filtrados.10
                                                       , FUN = median)



ggplot(varianza.elecciones.tipocambio.mediana.10, aes(x = dias_para_eleccion, y = varianza)) +
  geom_line(aes(group = 1)) +  labs(title = "Volatilidad media por días para elección 2010 - 2023")+ 
  theme(plot.title = element_text(hjust = 0.5))

varianza.elecciones.tipocambio.promedio.10 <- aggregate(varianza ~ dias_para_eleccion
                                                        , data=data.frame.retornos.filtrados.10
                                                        , FUN = mean)

ggplot(varianza.elecciones.tipocambio.promedio.10, aes(x = dias_para_eleccion, y = varianza)) +
  geom_line(aes(group = 1)) +  labs(title = "Volatilidad promedio por días para elección 2010 - 2023")+ 
  theme(plot.title = element_text(hjust = 0.5))
