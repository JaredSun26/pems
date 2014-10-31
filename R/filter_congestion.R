



#' A function to apply congestion data filter on a dataset. The function removes the a day's datat if the running length of the congested data is shorter than 5 min. The congested data is defined as occ>con.occ. The default value of con.occ is 0.3
#' 
#' @param tables a list in which each element is a data dataframe for one day's traffic data. It could be the output of read.processed().
#' @param con.occ The lower bound of occupancy for congestion. The default value of con.occ is 0.3
#' @examples tables<-filter.missing(tables, con.occ=0.3)
#' @export filter.missing

filter.congestion<-function(tables, con.occ=0.3){
  tables<-tables[sapply(tables,function(x) max(rle(diff(which(as.numeric(rowMeans(x[,seq(5,ncol(x),by=2)]))>=con.occ)))$lengths)+1>=10)]
  
  return(tables)
}