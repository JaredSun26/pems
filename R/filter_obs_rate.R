#' A function to apply observation rate filter on dataset
#' 
#' @param tables a list in which each element is a data dataframe for one day's traffic data, the output of read.processed().
#' @param obs.rate The threshold of minimum daily obervation rate
#' @examples tables<-filter.obs.rate(tables, obs.rate=0.95)
#' @export filter.obs.rate

filter.obs.rate<-function(tables,obs.rate=0.95){
  #remove days with lower obs.rate
  tables<-tables[sapply(tables,function(x) nrow(x)>=2880*obs.rate)]
  return(tables) 
}
