#' Read processed PeMS data
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


#'Calulate the total flow and total occupancy accross lanes
#'
#'@param dataset The output from read.filtered(), a list of filtered data where each element correponds to one day's data
#'@examples 
#'datasets<-read.filtered(directory="./filtered/",  files="1213891-2011-09.*\\.csv")
#'datasets<-get.total(datasets)

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
