#' Caculate Drug Era
#'
#'
#'
#' @import dplyr
#' @import data.table
#' @param CasePharm data.frame with MemberID (chr),Desc (chr),DispenseDate (Date),DaysSupply (num)
#' @export
#' @examples
#' getDrugEra()
#'
getDrugEra<-function(CasePharm)
{
  ### Drug Era
  CasePharm<-CasePharm %>% arrange(MemberID,Desc,DispenseDate)
  CasePharm<-data.table(CasePharm)
  CasePharm[,diff:=c(NA,diff(DispenseDate)),by=list(Desc,MemberID)]
  CasePharm$DaysSupplyB<-c(NA,CasePharm$DaysSupply[-nrow(CasePharm)])
  CasePharm$DiffC<-CasePharm$diff-as.numeric(CasePharm$DaysSupplyB)
  CasePharm$NewEraB<-CasePharm$DiffC>30
  CasePharm[is.na(NewEraB)]$NewEraB<-T
  CasePharm[,DrugEra:=cumsum(NewEraB),by=list(Desc,MemberID)]
  ### Exposure day
  FLData<-CasePharm %>% group_by(Desc,MemberID,DrugEra) %>% filter(row_number()==1|row_number()==n())
  NofFData<-CasePharm[,list(NofRefills=.N), by=list(Desc,MemberID,DrugEra)]
  FLData<-inner_join(FLData,NofFData,by=c("Desc","MemberID","DrugEra"))
  FLDataNew<-inner_join(FLData %>% group_by(Desc,MemberID,DrugEra) %>%  filter(row_number()!=1),
                        FLData %>% group_by(Desc,MemberID,DrugEra) %>%  filter(row_number()==1) %>% select(Desc,MemberID,DrugEra,FirstDisDate=DispenseDate),
                        by=c("Desc","MemberID","DrugEra"))
  FLDataNew<-rbind(FLDataNew,
                   FLData %>% group_by(Desc,MemberID,DrugEra)%>% filter(row_number()==1) %>% mutate(FirstDisDate=DispenseDate))

  FLDataNew<-FLDataNew %>% group_by(Desc,MemberID,DrugEra) %>% arrange(desc(DispenseDate))%>%  filter(row_number()==1)

  FLDataNew$ExposureDays<-FLDataNew$DispenseDate-FLDataNew$FirstDisDate+as.numeric(FLDataNew$DaysSupply)

  FLDataNew
}
