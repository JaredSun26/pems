#'Calculate and convert total flow and occupancy to normalized form.
#'
#'@param dataset The output from read.filtered(), a list of filtered data where each element correponds to one day's data
#'@examples 
#'datasets<-read.filtered(directory="./filtered/",  files="1213891-2011-09.*\\.csv")
#'datasets<-get.total(datasets)
#'@export get.nor.total

get.nor.total <- function(datasets){
  get.nor.total1 <- function(dataset){
    # index modification (from 0-2879 to 1-2880)
    dataset$index = dataset$index + 1
    # number of columns
    n <- ncol(dataset)
    # number of lanes
    LL <- (n-3)/2
    # total traffic flow and occupancy
    tot.flow <- rowSums(dataset[, seq(4, n - 1, by = 2)])
    tot.occ <- rowSums(dataset[, seq(5, n, by = 2)])
    # normalized flow and occupancy
    nor.tot.flow <- scale(tot.flow)[,1]
    nor.tot.occ <- scale(tot.occ)[,1]
    # sample mean
    f.bar <- mean(tot.flow)
    o.bar <- mean(tot.occ)
    # sample standard deviation
    sf <- sd(tot.flow)
    so <- sd(tot.occ)
    # show the final list
    cbind(dataset[, 1:3], nor.tot.flow, nor.tot.occ, f.bar, o.bar, sf, so, LL)
  }
  lapply(datasets, FUN = get.nor.total1)
}
