#' A function to impute missing data using LOWESS based on data index
#' 
#' @param tables a list in which each element is a data dataframe for one day's traffic data The output of read.processed().
#' @param span The parameter in  loess which controls the degress of smoothing
#' @examples tables<-impute.missing(tables)
#' @export impute.missing

impute.missing<-function(tables,span=0.75){
  #impute missing data with lowess/spline/mean
  impute.missing1<-function(table){
    #generate full data frame with NA in flow and occ
    full.index<-match(0:2879, table$index)
    na.index<-which(is.na(full.index))
    full<-table[full.index,]
    full[na.index,c(1,3)]=full[min(which(!is.na(full.index))),c(1,3)]
    full[na.index,2]=na.index-1
    
    #fill the holes with selected method
    impute<-function(y,index,na.index,span){
      y.loess<-loess(y~index,span=span)
      y.predict<-predict(y.loess, index)
      y[na.index]<-y.predict[na.index]
      return(y)
    }
    n=ncol(table)
    table<-cbind(full[,1:2],apply(full[,3:n],2,FUN=impute, index=full$index, na.index=na.index, span=span))
  }
  lapply(tables,FUN=impute.missing1)
}