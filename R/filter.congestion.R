#' A function to apply congestion data filter
#' 
#'The function removes the a day's datat if the running length of the congested data is shorter than 5 min. The congested data is defined as occ>con.occ. The default value of con.occ is 0.3
#' 
#' @param tables a list in which each element is a dataframe for one day's traffic data. It could be the output of read.processed().
#' @param con.occ The lower bound of occupancy for congestion. The default value of con.occ is 0.3
#' @param Minimum length of the qualified segment
#' @examples tables<-filter.missing(tables, con.occ=0.3, lgth=10)
#' @export filter.congestion

filter.congestion <- function (tables, con.occ=0.3, lgth=10){
  tables <- tables[sapply(tables, function(x) max(rle(diff(which(as.numeric(rowMeans(x[,seq(5,ncol(x),by=2)]))>=con.occ)))$lengths)+1>=lgth)]
  tables
}

