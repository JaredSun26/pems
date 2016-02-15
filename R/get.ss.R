#' A function to identify near-stationary states 
#' 
#' @param datasets The output from get.CI, a list of candidate intervals
#' @param mindr Minimum duration of the candidate intervals
#' @param maxdr Maximum duration of the candidate intervals
#' @param hot The high-occupancy threshold
#' @param gapt The gap threshold
#' @examples ss<-get.ss(ci,mindr=4, maxdr=10, hot=0.15, gapt=1)
#' @export get.ss


get.ss <- function(datasets, mindr, maxdr, hot, gapt){
  # create a empty near-stationary state list
  SS <- list()
  # for each candidate interval...
  for (dataset in datasets){
    # duration and gap criteria
    criteria  = (mean(dataset$avg.o) < hot & mean(dataset$dr) >= maxdr & max(mean(dataset$nor.fgap), mean(dataset$nor.ogap)) <= gapt/mean(dataset$sf)) |
      (mean(dataset$avg.o) >= hot & mean(dataset$dr) >= mindr & max(mean(dataset$nor.fgap), mean(dataset$nor.ogap)) <= gapt/mean(dataset$sf))
    # judgement
    if (criteria == TRUE) {SS[[length(SS)+1]] = dataset}
  }
  SS
}

