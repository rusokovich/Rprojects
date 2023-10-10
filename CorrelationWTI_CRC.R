## Correlacion Petroleo Anual
rm(list=ls())

# cargar los datos 

setwd('/Users/fkkarpuk/Downloads/')

# subir los datos, 

fuente<-read.csv('CorrelacionPetroleo.csv')

# Transformaciones

fuente$Fecha<-as.Date(fuente$Fecha)


#fuente <- fuente[complete.cases(fuente$PrecioPetroleo), ]
#fuente <- fuente[complete.cases(fuente$Tipo_Cambio_Venta), ]
fuente<- na.omit(fuente)
fuente <- fuente[order(fuente$Fecha), ]

str(fuente)

# Agrupar por Ano
Datos.anuales <- split(fuente, fuente$Ano)

# Correlaciones anuales Tipo Cambio Compra
correlaciones.anuales.compra <- lapply(Datos.anuales, function(f.correlacion.anual.compra) {
  cor(f.correlacion.anual.compra$PrecioPetroleo, f.correlacion.anual.compra$Tipo_Cambio_Compra)
})

df.correlaciones.compra<- do.call(rbind, correlaciones.anuales.compra)
df.correlaciones.compra<-as.data.frame(df.correlaciones.compra)
df.correlaciones.compra$Year<-rownames(df.correlaciones.compra)
colnames(df.correlaciones.compra)<-c("Correlation.Compra","Year")
df.correlaciones.compra <- df.correlaciones.compra[, c("Year", "Correlation.Compra")]


# Correlaciones anuales Tipo Cambio Venta
correlaciones.anuales.venta <- lapply(Datos.anuales, function(f.correlacion.anual.venta) {
  cor(f.correlacion.anual.venta$PrecioPetroleo, f.correlacion.anual.venta$Tipo_Cambio_Venta)
})

df.correlaciones.venta<- do.call(rbind, correlaciones.anuales.venta)
df.correlaciones.venta<-as.data.frame(df.correlaciones.venta)
df.correlaciones.venta$Year<-rownames(df.correlaciones.venta)
colnames(df.correlaciones.venta)<-c("Correlation.Venta","Year")
df.correlaciones.venta <- df.correlaciones.venta[, c("Year", "Correlation.Venta")]

df.consolidation.anual.correlation<-merge(df.correlaciones.venta,df.correlaciones.compra, by = "Year" )

# correlacion por politica cambiaria

# Agrupar por politica cambiaria 
Datos.politicas <- split(fuente, fuente$PoliticaCambiaria)

# Correlaciones politicas Tipo Cambio Compra
correlaciones.politicas.compra <- lapply(Datos.politicas, function(f.correlacion.politicas.compra) {
  cor(f.correlacion.politicas.compra$PrecioPetroleo, f.correlacion.politicas.compra$Tipo_Cambio_Compra)
})

df.correlaciones.politicas.compra<- do.call(rbind, correlaciones.politicas.compra)
df.correlaciones.politicas.compra<-as.data.frame(df.correlaciones.politicas.compra)
df.correlaciones.politicas.compra$Policy<-rownames(df.correlaciones.politicas.compra)
colnames(df.correlaciones.politicas.compra)<-c("Correlation.Compra","Policy")
df.correlaciones.politicas.compra <- df.correlaciones.politicas.compra[, c("Policy", "Correlation.Compra")]



# Correlaciones politicas Tipo Cambio Venta
correlaciones.politicas.venta <- lapply(Datos.politicas, function(f.correlacion.politicas.venta) {
  cor(f.correlacion.politicas.venta$PrecioPetroleo, f.correlacion.politicas.venta$Tipo_Cambio_Venta)
})

df.correlaciones.politicas.venta<- do.call(rbind, correlaciones.politicas.venta)
df.correlaciones.politicas.venta<-as.data.frame(df.correlaciones.politicas.venta)
df.correlaciones.politicas.venta$Policy<-rownames(df.correlaciones.politicas.venta)
colnames(df.correlaciones.politicas.venta)<-c("Correlation.Compra","Policy")
df.correlaciones.politicas.venta <- df.correlaciones.politicas.venta[, c("Policy", "Correlation.Compra")]

df.consolidation.policy.correlation<-merge(df.correlaciones.politicas.venta,df.correlaciones.politicas.compra, by = "Policy" )

