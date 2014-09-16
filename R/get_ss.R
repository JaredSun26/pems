
#' get steady state time sequence
#' 
#' @param datasets The output from get.total, a list of filtered data where each element correponds to one day's data
#' @param pen1 Penalty value used in PELT for flow data
#' @param pen2 Penalty value used in PELT for occupancy data
#' @param len The minimum duration of the steady state 
#' @examples ss<-get.ss(datasets,pen1=2,pen2=2)
get.ss<-function(datasets, pen1 = 2, pen2=2,len = 30){
  require(changepoint)
  ss<-list()
  for (dataset in datasets){
    #change NAN to 0 (0 flow devided by 0 occ)
    dataset$flow.occ[is.nan(dataset$flow.occ)]<-0
    #data normalization
    fit.cpt1<-cpt.mean(scale(dataset$total.flow)[,1], method="PELT",penalty="Manual", pen.value=paste(pen1,"*","log(n)",sep=""))
    fit.cpt2<-cpt.mean(scale(dataset$total.occ)[,1], method="PELT",penalty="Manual", pen.value=paste(pen2,"*","log(n)",sep=""))
    #merge two changepoints
    
    #split sequence into pieces according to cpts
    pos1<-head(fit.cpt1@cpts+1,-1)
    pos2<-head(fit.cpt2@cpts+1,-1)
    pos<-c(pos1,pos2)
    split.index<-unname(split(1:2880, cumsum(1:2880 %in% pos)))
    #select the intervals longer than len
    ss.index<-split.index[lapply(split.index,FUN=length)>=len]
    n<-length(ss.index)
    for (i in 1:n){
      ss[[length(ss)+1]]=dataset[ss.index[[i]],]
    }
  }
  return(ss)
}
