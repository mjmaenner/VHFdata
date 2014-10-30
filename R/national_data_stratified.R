

make_sx_national<-function(VHdata=VHFcase){
  require(tidyr)
  require(lubridate)
  
  recode_sx<-function(sxvar){
    sxvar_r<-ifelse(is.na(sxvar), 0, 
                    ifelse(sxvar==1, 1,0))
    return(sxvar_r)
  }
  
  sxnames<- names(VHdata)[c(62,63, 66:99)]
  sxnames_recode<-paste0(sxnames,"_r")
  
  for (i in 1:length(sxnames)){
    VHdata[,sxnames_recode[i]] <- recode_sx(VHdata[,sxnames[i]])
  }
  
  VHdata$total_initial_symptoms <- rowSums(VHdata[,sxnames_recode])
  VHdata$CaseStatus<-with(VHdata, ifelse(EpiCaseDef == 0, "Not a case",
                                         ifelse(EpiCaseDef == 1 , "Confirmed",
                                                ifelse(EpiCaseDef==2, "Probable",
                                                       ifelse(EpiCaseDef==3, "Suspect", NA)))))
  
  VHdata$cDateOutcome<-mdy_hm(VHdata$DateOutcomeComp)
  VHdata$dDateOutcome<-as.Date(mdy(paste(month(VHdata$cDateOutcome), day(VHdata$cDateOutcome), year(VHdata$cDateOutcome), sep="-")))
  VHdata$cDateOnset<-mdy_hm(VHdata$DateOnset)
  VHdata$dDateOnset<-as.Date(mdy(paste(month(VHdata$cDateOnset), day(VHdata$cDateOnset), year(VHdata$cDateOnset), sep="-")))
  VHdata$contact <- ifelse(VHdata$Contact == 1 & !is.na(VHdata$Contact), "Contact reported",
                           ifelse(VHdata$Funeral == 1 & !is.na(VHdata$Funeral), "Contact reported", "No contact reported"))
  
  VHdata$cDateReport<-mdy_hm(VHdata$DateReport)
  VHdata$dDateReport<-as.Date(mdy(paste(month(VHdata$cDateReport), day(VHdata$cDateReport), year(VHdata$cDateReport), sep="-")))
  assign("VHFcase_recode", VHdata ,envir=.GlobalEnv)
  VHFlong <- gather(data= VHFcase_recode[,c("ID","Gender","Age","CaseStatus","contact","total_initial_symptoms","StatusReport",sxnames_recode)], symptom, value, c(-Gender,-Age,-ID,-CaseStatus, -total_initial_symptoms, -StatusReport, -contact) )
  assign("VHFcase_long", VHFlong ,envir=.GlobalEnv)
}


VHFcp<-VHFcase_recode[ VHFcase_recode$CaseStatus %in% c("Confirmed","Probable"),]

library(ggplot2)

ggplot(data=VHFcp, aes(x=dDateReport, fill=StatusReport))+geom_histogram(binwidth=7, position="fill")+
  ggtitle(paste0("Confirmed and Probable Cases, Sierra Leone VHF\nby status when reported\n",now()))+
  theme_bw()+
  scale_fill_brewer(labels=c("Unknown","Dead","Alive"), type="qual",palette="Dark2", name="Status when\nreported")+
  scale_x_date(name="2014 Date (by week)")+scale_y_continuous("Proportion of weekly cases")
  
VHFalive<- VHFcase_recode[ VHFcase_recode$StatusReport == 2 & !is.na(VHFcase_recode$StatusReport) 
                           & VHFcase_recode$CaseStatus %in% c("Confirmed","Probable"),]

#2706 cases
#447 missing
library(lubridate)
VHFalive$onsettoreport<- interval(start = VHFalive$dDateOnset, end = VHFalive$dDateReport)/ddays(1)

