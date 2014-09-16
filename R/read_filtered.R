#' Read filtered PeMS data
#' 
#' This funtion is used to read filtered PeMS data in csv format with column: Data, Index, vd, flow, occ.
#' @param directory The directory where the csv files are located, default value is "./filtered/"
#' @param condition Regex of the file names, default value is "1213891-2011-09.*\\.csv"
#' @examples datasets<-read.filtered(directory="./filtered/",  files="1213891-2011-09.*\\.csv")

read.filtered<-function(directory="./filtered/", files="1213891-2011-09.*\\.csv"){
  #directory uses relative directory
  #files uses regex
  names<-list.files(directory)
  fnames<-names[grep(files,names)]
  if (length(fnames)==0){
    warning("No such files")
  }
  datasets<-lapply(paste(directory, fnames,sep=""), read.csv, header=T,as.is=T)
  return(datasets)
}