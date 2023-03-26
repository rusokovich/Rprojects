
# =================== FR ===============================

# Definir el area de trabajo

setwd('/Users/abc/Desktop/FR_Data')
path.files<-getwd()

# Definir los dataframes

files <- list.files(path=path.files, pattern="*.csv", full.names=TRUE, recursive=FALSE)

files.to.study <- list()
amount.files<-length(files)


for ( i in 1:amount.files ) {
  
  print(paste0("This file ", basename(files[i]), " contains ",ncol(read.csv(basename(files[i]),sep = ",",header = FALSE)) ," columns"))

  files.to.study[[i]] <- read.csv(basename(files[i]),sep = ",",header = FALSE)  ## create and add new data frame
}

# combinar los dataframes en 1 solo que se llamara df



df<-Reduce(function(x,y) merge(x, y, all=TRUE), files.to.study, accumulate=FALSE)

colnames(df)<-make.names(c("Ring Group Name",
                           "Timestamp",
                           "Service Level",
                           "Total Inbound Calls",
                           "Calls Answered Within Service Level",
                           "Calls Missed Within Service Level",
                           "Inbound Calls Answered",
                           "Inbound Calls Missed",
                           "Inbound Calls Abandoned",
                           "AVG Talk Time",
                           "AVG Hold Time",
                           "AVG Total Duration",
                           "Talk Time",
                           "Hold Time",
                           "Total Duration"))

# arrange the dates
df$Timestamp <- as.POSIXct(df$Timestamp, format = "%Y-%m-%d %H:%M:%S") 
df<-df[order(df$Timestamp),]
df$Timestamp<-as.character(df$Timestamp)

# Remove last 2 rows
n<-dim(df)[1]
df<-df[1:(n-2),]

write.csv(df,"/Users/abc/Desktop/FRWO_SL.csv", row.names = FALSE)




# =================== SP ===============================
# Definir el area de trabajo

setwd('/Users/abc/Desktop/SP_Data')
path.files<-getwd()

# Definir los dataframes

files <- list.files(path=path.files, pattern="*.csv", full.names=TRUE, recursive=FALSE)

files.to.study <- list()
amount.files<-length(files)


for ( i in 1:amount.files ) {
  
  print(paste0("This file ", basename(files[i]), " contains ",ncol(read.csv(basename(files[i]),sep = ",",header = FALSE)) ," columns"))
  
  files.to.study[[i]] <- read.csv(basename(files[i]),sep = ",",header = FALSE)  ## create and add new data frame
}

# combinar los dataframes en 1 solo que se llamara df



df<-Reduce(function(x,y) merge(x, y, all=TRUE), files.to.study, accumulate=FALSE)

colnames(df)<-make.names(c("Ring Group Name",
                           "Timestamp",
                           "Service Level",
                           "Total Inbound Calls",
                           "Calls Answered Within Service Level",
                           "Calls Missed Within Service Level",
                           "Inbound Calls Answered",
                           "Inbound Calls Missed",
                           "Inbound Calls Abandoned",
                           "AVG Talk Time",
                           "AVG Hold Time",
                           "AVG Total Duration",
                           "Talk Time",
                           "Hold Time",
                           "Total Duration"))

# arrange the dates
df$Timestamp <- as.POSIXct(df$Timestamp, format = "%Y-%m-%d %H:%M:%S") 
df<-df[order(df$Timestamp),]
df$Timestamp<-as.character(df$Timestamp)

# Remove last 2 rows
n<-dim(df)[1]
df<-df[1:(n-2),]

write.csv(df,"/Users/abc/Desktop/SP_SL.csv", row.names = FALSE)


# =================== EN ===============================
# Definir el area de trabajo

setwd('/Users/abc/Desktop/EN_Data')
path.files<-getwd()

# Definir los dataframes

files <- list.files(path=path.files, pattern="*.csv", full.names=TRUE, recursive=FALSE)

files.to.study <- list()
amount.files<-length(files)


for ( i in 1:amount.files ) {
  
  print(paste0("This file ", basename(files[i]), " contains ",ncol(read.csv(basename(files[i]),sep = ",",header = FALSE)) ," columns"))
  
  files.to.study[[i]] <- read.csv(basename(files[i]),sep = ",",header = FALSE)  ## create and add new data frame
}

# combinar los dataframes en 1 solo que se llamara df



df<-Reduce(function(x,y) merge(x, y, all=TRUE), files.to.study, accumulate=FALSE)

colnames(df)<-make.names(c("Ring Group Name",
                           "Timestamp",
                           "Service Level",
                           "Total Inbound Calls",
                           "Calls Answered Within Service Level",
                           "Calls Missed Within Service Level",
                           "Inbound Calls Answered",
                           "Inbound Calls Missed",
                           "Inbound Calls Abandoned",
                           "AVG Talk Time",
                           "AVG Hold Time",
                           "AVG Total Duration",
                           "Talk Time",
                           "Hold Time",
                           "Total Duration"))

# arrange the dates
df$Timestamp <- as.POSIXct(df$Timestamp, format = "%Y-%m-%d %H:%M:%S") 
df<-df[order(df$Timestamp),]
df$Timestamp<-as.character(df$Timestamp)

# Remove last 2 rows
n<-dim(df)[1]
df<-df[1:(n-2),]

write.csv(df,"/Users/abc/Desktop/EN_SL.csv", row.names = FALSE)



# =================== Schedules ===============================
# librerias necesarias

library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(data.table)
library(tibble)

# Definir el area de trabajo

setwd('/Users/abc/Desktop/HistoricalSegments')
path.files<-getwd()

# Definir los dataframes

files <- list.files(path=path.files, pattern="*.csv", full.names=TRUE, recursive=FALSE)

files.to.study <- list()
amount.files<-length(files)


for ( i in 1:amount.files ) {
  
  print(paste0("This file ", basename(files[i]), " contains ",ncol(read.csv(basename(files[i]),sep = ",",header = FALSE)) ," columns"))
  
  files.to.study[[i]] <- read.csv(basename(files[i]),sep = ",",header = FALSE)  ## create and add new data frame
}

# crear el documento unificado y asignar la primera fila como columna 
df<-Reduce(function(x,y) merge(x, y, all=TRUE), files.to.study, accumulate=FALSE)
names(df) <- make.names(df[1,])
df <- df[-1,]

# cargar los datos desde la API

endpoint <- "https://abc.cloud.agyletime.io/api/authority/token" # nolint
identifier <- 'user_code'
secret <- 'encryptedKey'


response <- POST(endpoint, config = authenticate(identifier , secret))


# Parse the response to extract the bearer token
#print(content(response))
bearer_token=content(response)$data$accessToken


api.value <-"/schedule/tasks/definitions"
url<-"https://sendwave.cloud.agyletime.io/api"
#url<-"https://api-us.cloud.agyletime.io"

res <- GET( 
  url = paste0(url,api.value), 	
  add_headers(Authorization = paste0("Bearer ", bearer_token)))

res[["status_code"]]

myparsed.content<-content(res, 'text')
json.content<-fromJSON(myparsed.content)


activity_data<- json.content$data

activity_data<-activity_data %>% select(c('name','isPaid','isPlanned','isLeave','isBreak','categoryName','workloadChannel'))

# merging data 
merged_df<- df %>% left_join(activity_data,by=c("Task.Name"="name"))

# removing columns 

merged_df<-merged_df %>% select(-c("Date","Shift.Comment","Task.Comment"))

# removing NA for blank only 

merged_df[is.na(merged_df)] <- ""

write.csv(merged_df,"/Users/abc/Desktop/HistoricalSchedules.csv", row.names = FALSE)

