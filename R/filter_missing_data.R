#' A function to apply missing data filter on a dataset
#' 
#' @import changepoint
#' @param tables a list in which each element is a data dataframe for one day's traffic data. It could be the output of read.processed().
#' @param hole.size The threshold for the longest NA sequence
#' @examples tables<-filter.missing(tables, hole.size=6)
#' @export filter.missing

filter.missing<-function(tables, hole.size=6){
  #remove days with large holes
  tables<-tables[sapply(tables,function(x) max(diff(c(-1,x$index,2880))<=hole.size))]
  return(tables)
}