#' Get the first diagnosis record
#'
#' This can be used to select the first diagnosis record
#' based on ICD code (grepl language) in factIcd Data,
#' return first diagnosis record based on factIcd Data
#'
#' @param icdList
#' @param groupingTable
#' @export
#' @examples
#' groupICDBasedOnGrep(icdList,groupingTable)
#'

groupICDBasedOnGrep<-function(icdList,groupingTable){
  Group<-character(length(icdList))
  for(i in 1:nrow(groupingTable)){
    Group<-ifelse(grepl(groupingTable$grep_pattern[i],tolower(icdList)),
                  groupingTable$label[i],Group)
  }
  Group
}
