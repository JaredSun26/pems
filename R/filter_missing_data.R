#' A function to apply missing data filter on dataset
#' 
#' @param tables a list in which each element is a data dataframe for one day's traffic data, the output of read.processed().
#' @param hole.size The threshold for the longest NA sequence
#' @examples tables<-filter.missing(tables, hole.size=6)

filter.missing<-function(tables, hole.size=6){
  #remove days with large holes
  tables<-tables[sapply(tables,function(x) max(diff(x$index))<=hole.size)]
  return(tables)
}