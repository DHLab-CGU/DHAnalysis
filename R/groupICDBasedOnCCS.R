#' Get the first diagnosis record
#'
#' This can be used to select the first diagnosis record
#' based on ICD code (grepl language) in factIcd Data,
#' return first diagnosis record based on factIcd Data
#'
#' @import stringr
#' @import icd
#' @param icdList
#' @export
#' @examples
#' groupICDBasedOnCCS(icdList)
#'

groupICDBasedOnCCS<-function(icdList){
  Group<-character(length(icdList))
  #icd9
  Group[icd_is_valid(icdList)]<-
    left_join(data.frame(ICDD=icdList[icd_is_valid(icdList)],stringsAsFactors = F),select(ccsICD9,ICDD,CCSCate),all.x=T,by = "ICDD")$CCSCate
  #icd10
  Group[!icd_is_valid(icdList)]<-
    left_join(data.frame(ICDD=icdList[!icd_is_valid(icdList)],stringsAsFactors = F),select(ccsICD10,ICDD,CCSCate),all.x=T, by = "ICDD")$CCSCate
  str_trim(Group)
}
