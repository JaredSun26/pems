#' A function to scrape data from PeMS clearing house
#' 
#' @param place holder
#' @param place holder
#' @examples place holder
#' @export place holder

download_pems<-function(){
  require(httr)
  require(XML)
  
  #test the data scrapping function
  yy=2014
  mm=1
  day=2
  disId=12
  dataType="station_raw"
  ur="jaredsun26@gmail.com"
  ps="javaw8"
  destfile=paste0(dataType,"_","d",
                  disId,"_",
                  day,"-",
                  mm,"-",
                  yy,
                  ".txt.gz"
  )
  library(httr)
  library(XML)
  
  # fields found in the login form.
  login <- list(
    redirect="",
    username = ur,
    password = ps,
    login="Login"
  )
  
  #send authentication information
  message("Sending login information...")
  response<-POST("http://pems.dot.ca.gov", body=login)
  if (response$status_code==200){
    message("Success")
  }else{
    stop("Connection Failed, status code:", response$status_code)
  }
  
  #get list of file download URL (json format)
  ##URL for the file lists
  listURL=paste0("http://pems.dot.ca.gov/?srq=clearinghouse&district_id=",disId,"&yy=", yy, "&type=", dataType, "&returnformat=text")
  fileList<-content(GET(url=listURL,body=login),
                    "parsed", 
                    type="application/json")
  fileURL=paste0("http://pems.dot.ca.gov/", fileList$data[[mm]][[day]]$url)
  #download the file
  message("downloadig the file...")
  downloaded<-GET(url=fileURL)
  #write the file to disk
  message("writing to files")
  writeBin(content(downloaded, "raw"),destfile)
  message("success")
}