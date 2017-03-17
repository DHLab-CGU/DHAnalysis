#' Get the first diagnosis record
#'
#' This can be used to select the first diagnosis record
#' based on ICD code (grepl language) in factIcd Data,
#' return first diagnosis record based on factIcd Data
#'
#' @import icd
#' @param icdList
#' @export
#' @examples
#' getICDName(icdList)
#'

getICDName<-function(icdList){
  isICD9<-icd_is_defined(icdList)
  IcdDesc<-character(length(icdList))
  IcdDesc[isICD9==T]<-icd_explain_table(icdList[isICD9==T])$long_desc
  IcdDesc[isICD9==F]<-icd_explain_table(icdList[isICD9==F])$long_desc
  IcdDesc
}
