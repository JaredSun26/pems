#' Parse Pems raw data and pick a subset of VDS
#' 
#' This funtion reads in the pems data and reform it into a "comfortable" format with columns: Time, vds, flow, occ, speed.
#' @param inDir The directory where the pems raw files are located, the raw files are in .gz format 
#' @param outDir The directory to write the output files, the files are in .csv format
#' @param vds A vector of desired VDS's
#' @param meta The meta data used to parse the data. It is suggested to read in the meta file by function "read.delim"
#' @examples meta <- read.delim("./meta/d12_text_meta_2013_12_14.txt")
#' parse.pems(meta=meta)
#' @export parse.pems

parse.pems<-function(inDir="./pemsData/",  outDir="./processed/", vds=c(1203501,1203506,1203534,1203536,1212126,1213891),meta){
  fnames=list.files(inDir)
  n=length(fnames)
  pb<-txtProgressBar(max=n,style=3)
  i=0
  #a kernel function to read VDS
  queryVDS<-function(data,meta,vdsList,date,outDir){
    #construct (vds,lanes) mapping
    vds.lane<-as.matrix(meta[meta$ID %in% vdsList,c(1,13)])
    #extract vds flow data by 
    extractData<-function(vds.row){
      #vds ID
      id=vds.row[1]
      #number of lanes
      n=vds.row[2]
      prefix=c("flow","occ","speed")
      vds.data<-data[data$V2==id,1:(3*n+2)]
      colnames(vds.data)<-c("time","vds",as.vector(outer(prefix, 1:n, paste,
                                                         sep=".")))
      write.csv(file=paste(outDir,id, date, ".csv",sep=""),vds.data,quote=F,row.names=F)
    }
    apply(vds.lane,1,extractData)
  }
  
  for (name in fnames){
    cdate=substr(name,21,31)
    data<-read.table(gzfile(paste(inDir,name,sep="")),sep=",")
    queryVDS(data,meta,vds, cdate,outDir)
    i=i+1
    setTxtProgressBar(pb,i)
  }
}
