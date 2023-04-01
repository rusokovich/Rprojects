pollutantmean<-function(directory,pollutant,id=1:322){
    setwd("C:/Users/User/Desktop/Data base Course/R scripts/Assesments/First_Assesment")
    data_pathw<-paste(getwd(),"/",directory,sep = "")
    data_pathr<-gsub("/","\\",data_pathw,fix=TRUE)
    filenames<-list.files(data_pathr)
    nums<-c()
    for (i in filenames[id]) {
        anlz_fl<-read.csv(paste(data_pathr,"\\",i,sep=""))
        anlz_col<-anlz_fl[pollutant]
        data<-anlz_col[complete.cases(anlz_col),]
        nums<-c(nums,data)
    }
    print(mean(nums))
}
