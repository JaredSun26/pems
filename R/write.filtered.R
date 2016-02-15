#' A function write the filtered data into csv file
#' 
#' @param tables a list in which each element is a data dataframe for one day's traffic data, the output of read.processed().
#' @examples  write.filtered(tables)
#' @export write.filtered



write.filtered<-function(tables,outDir="./filtered/"){
  for (table in tables){
    id=paste(table[1,3],"-",sep="")
    date=table[1,1]
    write.csv(file=paste(outDir,id, date, ".csv",sep=""), table, quote=F, row.names=F)
  }
}