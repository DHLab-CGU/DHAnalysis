#' Get the first diagnosis record
#'
#' This can be used to select the first diagnosis record
#' based on ICD code (grepl language) in factIcd Data,
#' return first diagnosis record based on factIcd Data
#'
#' @param caseAllIcd
#' @param caseFirstIcd
#' @export
#' @examples
#' getDiagPeriod(caseAllIcd,caseFirstIcd)
#'

getDiagPeriod<-function(caseAllIcd,caseFirstIcd){
  ## Period
  caseAllIcd<-merge(caseAllIcd,caseFirstIcd,all.x = T,by="MemberID") #5780702
  caseAllIcd$DateServiceStarted<-as.Date(caseAllIcd$DateServiceStarted)
  caseAllIcd$Date<-as.Date(caseAllIcd$Date)
  caseAllIcd$Gap<-caseAllIcd$DateServiceStarted-caseAllIcd$Date
  caseAllIcd$Period<-ifelse(caseAllIcd$Gap>=0,"After","Before")
  caseAllIcd
}
