#' Get the gap between event date and first diagnosis date
#'
#' This can be used to caculate the gap between event date (such as dispense date, enconter date) and the first diagnosis date
#'
#' @import dplyr
#' @param eventAllDate data.frame with MemberID as first column and EventDate as second column
#' @param caseFirstDate data.frame with MemberID and FirstDate
#' @export
#' @examples
#' getEventPeriod(eventAllDate,caseFirstDate)
#'

getEventPeriod<-function(eventAllDate,caseFirstDate){
  colnames(eventAllDate)<-c("MemberID","EventDate")
  eventAllDate<-left_join(eventAllDate,caseFirstDate,all.x = T,by="MemberID") #5780702
  eventAllDate$EventDate<-as.Date(eventAllDate$EventDate)
  eventAllDate$FirstDate<-as.Date(eventAllDate$FirstDate)
  eventAllDate$Gap<-eventAllDate$EventDate-eventAllDate$FirstDate
  eventAllDate$Period<-ifelse(eventAllDate$Gap>=0,"After","Before")
  eventAllDate
}
