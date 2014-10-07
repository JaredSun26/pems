#' Read processed PeMS data
#' 
#' This funtion is used to read processed PeMS data in csv format (columns: Time, vds, flow, occ, speed) and add date column, convert time to index and remove speed column.
#' @param directory The directory where the csv files are located, default value is "./processed/"
#' @param condition Regex of the file names, default value is "1213891__2011_09_.*\\.csv"
#' @examples datasets<-read.processed()

read.processed<-function(directory="./processed/", condition="1213891__2011_09_.*\\.csv"){
  names<-list.files(directory)
  fnames<-names[grep(condition,names)]
  if (length(fnames)==0){
    warning("No such files")
  }
  tables<-lapply(paste(directory, fnames,sep=""), read.csv, header=T,as.is=T)

  #function to add date, convert time to index
  clean.table<-function(table){
    
    #add date
    dt<-strptime(table$time ,format="%m/%d/%Y %H:%M:%S")
    date<-strftime(trunc(dt,"days"))
    #convert time to index (range=0 to 2879)
    index<-(as.numeric(dt)-28800)%%86400/30
    
    #put things together and remove unnessesary columns
    n=ncol(table)
    table<-cbind(date,index ,table[,c(-1,-seq(5,n,by=3))])
    return(table)
  }
  
  tables<-lapply(tables, FUN=clean.table)
  #return the result
  return(tables)
}




  
