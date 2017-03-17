#' Get the first diagnosis record
#'
#' This can be used to select the first diagnosis record
#' based on ICD code (grepl language) in factIcd Data,
#' return first diagnosis record based on factIcd Data
#'
#' @import dplyr
#' @param greplString ICD selection rules with grepl expression
#' @param factIcd ICD dataset with at least 3 columns: "MemberID","Date","ICD"
#' @export
#' @examples
#' getFirstDiagBasedOnFactIcdList('255|256',sampleIcdData)
#'
getFirstDiagBasedOnFactIcdList<-function(greplString,factIcd){

  caseFirstIcd<-factIcd %>% filter(grepl(greplString,ICD)) %>%
    group_by(MemberID) %>% arrange(Date) %>% filter(row_number() == 1)
  caseFirstIcd
}
