#'Calulate the total flow and total occupancy accross lanes
#'
#'@param dataset The output from read.filtered(), a list of filtered data where each element correponds to one day's data
#'@examples 
#'datasets<-read.filtered(directory="./filtered/",  files="1213891-2011-09.*\\.csv")
#'datasets<-get.total(datasets)
#'@export get.total

get.total<-function(datasets){
  get.total1<-function(dataset){
    n<-ncol(dataset)
    total.flow<-rowSums(dataset[,seq(4,n-1,by=2)])
    total.occ<-rowSums(dataset[,seq(5,n,by=2)])
    flow.occ<-total.flow/total.occ
    return(cbind(dataset[,1:3], total.flow, total.occ, flow.occ))
  }
  return(lapply(datasets,FUN=get.total1))
}