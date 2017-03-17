#' Get the first diagnosis record
#'
#' This can be used to select the first diagnosis record
#' based on ICD code (grepl language) in factIcd Data,
#' return first diagnosis record based on factIcd Data
#'
#' @import dplyr
#' @param caseFirstIcd
#' @param memFirstEnro
#' @param washoutDay
#' @param followupDay
#' @export
#' @examples
#' getBeforeAndAfterDate(caseFirstIcd,memFirstEnro,365,365)
#'
getBeforeAndAfterDate<-function(caseFirstIcd,memFirstEnro,washoutDay,followupDay){
  caseMem<-merge(caseFirstIcd,memFirstEnro,by = "MemberID",all=T)
  caseMem<-data.table(caseMem)
  caseMem$EffectiveDate<-as.Date(caseMem$EffectiveDate)
  caseMem$CancelDate<-as.Date(caseMem$CancelDate)
  caseMem$Date<-as.Date(caseMem$Date)
  caseMem$Washout<-caseMem$Date-caseMem$EffectiveDate
  caseMem$Followup<-caseMem$CancelDate- caseMem$Date
  filter(caseMem,Washout>washoutDay & Followup>followupDay)
  #caseMem[caseMem$Washout>washoutDay & (caseMem$Followup>followupDay),]
}
