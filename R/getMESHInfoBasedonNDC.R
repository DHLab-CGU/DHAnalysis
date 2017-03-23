#' Get MESH info based on NDC code
#'
#' This can be used to get MESH information based on NDC codes by RxNav API (https://rxnav.nlm.nih.gov)
#'
#' @import jsonlite
#' @import doParallel
#' @param NDCList Vector of NDC code in 11 digits format
#' @param cores Number of cores you want to used
#' @export
#' @examples
#' NationalDrugCodeList<-c("54092047612","00430078327","00406035705","00074433902","00310075190")
#' getMESHInfoBasedonNDC(NationalDrugCodeList)
#'
getMESHInfoBasedonNDC<-function(NDCList,cores=8){

  cl <- makeCluster(cores) # create a cluster with n cores
  registerDoParallel(cl) # register the cluster
  RxNormData = foreach(i = 1:length(NDCList),
                       .combine = "rbind",
                       .packages = "jsonlite") %dopar% {
                         RxNorm<-fromJSON(paste0("https://rxnav.nlm.nih.gov/REST/rxcui?idtype=NDC&id=",NDCList[i]) )
                         if(is.null(RxNorm$idGroup$rxnormId)){
                           rxTable<-data.frame(NDC=NDCList[i],
                                               RxCui="",
                                               Desc="",Type="")
                         }else{
                           MESHClass<-fromJSON(paste0("https://rxnav.nlm.nih.gov/REST/rxclass/class/byRxcui?rxcui=",RxNorm$idGroup$rxnormId,"&relaSource=MESH"))
                           if(is.null(MESHClass$rxclassDrugInfoList$rxclassDrugInfo$minConcept$name)){
                             rxTable<-data.frame(NDC=NDCList[i],
                                                 RxCui=RxNorm$idGroup$rxnormId,
                                                 Desc="",Type="")
                           }else{
                             meshTable<-data.frame(NDC=NDCList[i],
                                                   RxCui=RxNorm$idGroup$rxnormId,
                                                   Desc=unique(MESHClass$rxclassDrugInfoList$rxclassDrugInfo$rxclassMinConceptItem$className),
                                                   Type="MESH")
                             miniTable<-data.frame(NDC=NDCList[i],
                                                   RxCui=RxNorm$idGroup$rxnormId,
                                                   Desc=unique(MESHClass$rxclassDrugInfoList$rxclassDrugInfo$minConcept$name),
                                                   Type="miniConcept")
                             rxTable<-rbind(meshTable,miniTable)
                           }
                         }
                         rxTable
                       }
  stopCluster(cl)
  RxNormData
}
