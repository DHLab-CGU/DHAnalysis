#' Select cases based on ICD code and the number of ICD codes
#'
#' This can be used to select qualified cases from factIcd data
#' based on the ICD code searching criteria and number of ICD code
#' per patients in the inout factIcd dataset. Return MemberID
#'
#' @import dplyr
#' @param greplString ICD selection rules with grepl expression
#' @param factIcd ICD dataset with at least 3 columns: "MemberID","Date","ICD"
#' @param ICDNumber a threshold of number of ICD for case selection
#' @export
#' @examples
#' selectCasesBasedOnFactIcdList('255|256',sampleIcdData,2)
#'
selectCasesBasedOnFactIcdList<-function(greplString,factIcd,ICDNumber){

  MemCount<-factIcd %>% filter(grepl(greplString,ICD)) %>% group_by(Date,MemberID) %>%
    summarise(nDate=n()) %>% arrange(desc(nDate))

  Cases<-MemCount %>% group_by(MemberID) %>% summarise(nDiag=n()) %>% arrange(desc(nDiag))%>% filter(nDiag>1)

  unique(Cases$MemberID)

}
