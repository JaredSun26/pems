#' Get candidate intervals given a specific penalty
#' 
#'  This function derives characteristics of candidate intervals, including starding and ending point positions, starting and ending time, length, duration, total flow-rate, average occupancy, normalized flow and occupancy gaps.
#' 
#' @param datasets The output from get.total, a list of filtered data where each element correponds to one day's data.
#' @param pen Penalty value used in PELT for flow data and occupancy data.
#' @examples ci<-get.CI(datasets,pen1=0.01)
#' @export get.ci



get.ci<-function(datasets, pen){
  # load changepoint package
  require(changepoint)
  # create a blank candidate interval set
  CI <- list()
  # calculate the data aggregation interval (30s in our case)
  theta <- 24*60*60/nrow(datasets[[1]])
  # for each day...
  for (dataset in datasets){
    # changes in mean
    fit.f <- cpt.mean(dataset$nor.tot.flow, method = "PELT", penalty = "Manual", pen.value=paste(pen,"*","log(n)",sep=""))
    fit.o <- cpt.mean(dataset$nor.tot.occ, method = "PELT", penalty = "Manual", pen.value=paste(pen,"*","log(n)",sep=""))
    # changepoints for flow and occupancy
    cpt.f <- fit.f@cpts
    cpt.o <- fit.o@cpts
    # combined changepoint set (include 0)
    cpt = sort(Reduce(union, list(cpt.f, cpt.o, 0)))

    # number of candidate intervals
    nci <-  length(cpt) - 1   
    
    # for each cs ...
    for (k in 1:nci){
      # starting and ending point positions
      sp = cpt[k]+1
      ep = cpt[k+1]
      # starting and ending time
      st = 1/3600*theta*(sp-1)
      et = 1/3600*theta*(ep-1)
      # length
      le = ep - sp + 1
      # duration
      dr = 1/60*theta*le
      # obtain the previous information
      info = dataset[sp:ep,]
      # total flow-rate and average occupancy
      tot.q = 60/dr*sum(info$sf*info$nor.tot.flow+info$f.bar)  
      avg.o = 1/(le*info$LL)*sum(info$so*info$nor.tot.occ+info$o.bar)
      # normalized cumulative flows and occupancies
      nor.cumf = cumsum(info$nor.tot.flow)
      nor.cumo = cumsum(info$nor.tot.occ)
      # normalized flow and occupancy gaps
      nor.fgap = max(abs(nor.cumf - fitted.values(lm(nor.cumf ~ c(sp:ep)))))
      nor.ogap = max(abs(nor.cumo - fitted.values(lm(nor.cumo ~ c(sp:ep)))))
      # combine them together
      CI[[length(CI)+1]] <-  cbind(info, sp, ep, st, et, le, dr, tot.q, avg.o, nor.fgap, nor.ogap)
    }
  }
  CI
}






