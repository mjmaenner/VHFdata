### description of initial symptoms among probable and confirmed cases


make_symptom_variables<-function(VHdata=VHFcase){
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
  
  VHdata$cDateOutcome<-mdy_hms(VHdata$DateOutcomeComp)
  VHdata$dDateOutcome<-as.Date(mdy(paste(month(VHdata$cDateOutcome), day(VHdata$cDateOutcome), year(VHdata$cDateOutcome), sep="-")))
  
  VHdata$cDateReport<-mdy_hms(VHdata$DateReport)
  VHdata$dDateReport<-as.Date(mdy(paste(month(VHdata$cDateReport), day(VHdata$cDateReport), year(VHdata$cDateReport), sep="-")))
  
  VHdata$cDateOnset<-mdy_hms(VHdata$DateOnset)
  VHdata$dDateOnset<-as.Date(mdy(paste(month(VHdata$cDateOnset), day(VHdata$cDateOnset), year(VHdata$cDateOnset), sep="-")))
  VHdata$contact <- ifelse(VHdata$Contact == 1 & !is.na(VHdata$Contact), "Contact reported",
                           ifelse(VHdata$Funeral == 1 & !is.na(VHdata$Funeral), "Contact reported", "No contact reported"))
  
  VHdata$onsettoreport<- interval(start = VHdata$dDateOnset, end = VHdata$dDateReport)/ddays(1)
  assign("VHFcase_recode", VHdata ,envir=.GlobalEnv)
  VHFlong <- gather(data= VHFcase_recode[,c("ID","Gender","Age","CaseStatus","contact","total_initial_symptoms","StatusReport",sxnames_recode)], symptom, value, c(-Gender,-Age,-ID,-CaseStatus, -total_initial_symptoms, -StatusReport, -contact) )
  assign("VHFcase_long", VHFlong ,envir=.GlobalEnv)
}

VHFcase_date<-function(VHdata=VHFcase_recode){
  
  VHdata$CIF<-mdy_hms(VHdata$DateReport)
  VHdata$Onset<-mdy_hms(VHdata$DateOnset)
  VHdata$HospitalAdmit<-mdy_hms(VHdata$DateHospitalCurrentAdmit)
  VHdata$Isolation <- mdy_hms(VHdata$DateIsolationCurrent)
  VHdata$Death <-mdy_hms(VHdata$DateDeath)
  VHdata$DischargeIso <-mdy_hms(VHdata$DateDischargeIso)
  VHdata$DischargeHosp <-mdy_hms(VHdata$DateDischargeHosp)
  VHdata$LabDraw <-mdy_hms(VHdata$DateSampleCollected1)
  VHdata$LabTested <-mdy_hms(VHdata$DateSampleTested1)
  datevars<-c("CIF", "Onset","HospitalAdmit", "Isolation","Death","DischargeIso",
              "DischargeHosp", "LabDraw","LabTested")
  VHdata_long <- gather(data= VHdata[,c("ID","Gender","Age","CaseStatus","contact","cDateOnset", "total_initial_symptoms","StatusReport",datevars)], eventdate, value, c(-Gender,-Age,-ID,-CaseStatus, -total_initial_symptoms, -StatusReport, -contact, -cDateOnset) )
  assign("VHFdate_long", VHdata_long ,envir=.GlobalEnv)
}

ggplot(data=VHFdate_long, aes(x=value, y=reorder(ID,cDateOnset ), group=ID))+
  geom_line()+
  geom_point(aes(colour=eventdate))+theme_minimal()+
  scale_colour_brewer(type="qual", palette="Set1")+
  theme(axis.line= element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), panel.grid.major=element_blank())


sx_frequency_overall <- function(VHFlongdata=VHFcase_long){
  require(ggplot2)
  require(dplyr)
  require(stringr)
  require(lubridate)
  VHFoverall<- VHFlongdata %>%
    group_by(symptom) %>%
    summarise(proportion=mean(value)) %>%
    mutate(Symptom = str_replace(symptom, "_r",""),
           Symptom = reorder(Symptom,proportion))
  print(
    ggplot(data=VHFoverall, aes(x=Symptom, y=proportion*100))+
      geom_point(size=4)+ geom_bar(stat="identity", width=0.2)+
      coord_flip()+theme_bw()+
      scale_y_continuous("Percent of Susp/Prob/Conf Cases", breaks=seq(0,100,10))+
      ggtitle(paste("Overall initial symptom frequency\nProduced on",now())))
}


sx_frequency_overall_complete <- function(VHFlongdata=VHFcase_long){
  require(ggplot2)
  require(dplyr)
  require(stringr)
  require(lubridate)
  VHFoverall<- VHFlongdata %>%
    group_by(symptom) %>%
    filter( total_initial_symptoms > 0) %>%
    filter( StatusReport == 2 ) %>%
    summarise(proportion=mean(value)) %>%
    
    mutate(Symptom = str_replace(symptom, "_r",""),
           Symptom = reorder(Symptom,proportion))
  
  ggplot(data=VHFoverall, aes(x=Symptom, y=proportion*100))+
    geom_point(size=4)+ geom_bar(stat="identity", width=0.2)+
    coord_flip()+theme_bw()+
    scale_y_continuous("Percent of Susp/Prob/Conf Cases", breaks=seq(0,100,10))+
    ggtitle(paste("Overall symptom frequency, alive at time of Report\nProduced on",now()))
}



sx_frequency_gender <- function(VHFlongdata=VHFcase_long){
  require(ggplot2)
  require(dplyr)
  require(stringr)
  require(reshape2)
  require(lubridate)
  VHFoverall<- VHFlongdata %>%
    filter( StatusReport == 2  &  !is.na(StatusReport)) %>%
    filter( CaseStatus %in% c("Confirmed","Probable")) %>%
    group_by( symptom, Gender) %>%
    summarise(proportion=mean(value)) 
  
  VHFoverall$Symptom <-str_replace(VHFoverall$symptom, "_r","") 
  VHFoverall$Symptom = reorder(VHFoverall$Symptom,VHFoverall$proportion)
  VHFoverall$gender = factor(VHFoverall$Gender)
  
  
  
  ggplot(data=VHFoverall[is.finite(VHFoverall$gender),], aes(x=Symptom, y=proportion*100, group=gender, fill=gender, colour=gender))+
    #geom_bar(position="dodge")+
    geom_point()+
    coord_flip()+theme_bw()+
    scale_y_continuous("Percent of Susp/Prob/Conf Cases", breaks=seq(0,100,10))+
    ggtitle(paste("Initial symptom frequency by gender\nProduced on",now()))
  return(assign("VHFcase_sx_gender", VHFoverall, envir=.GlobalEnv))
}




sx_frequency_genderscatter<-function(VHFoverall=VHFcase_sx_gender)
{
  VHFlong.male <- data.frame(VHFoverall[VHFoverall$gender==1 & is.finite(VHFoverall$gender),])
  VHFlong.male$male_pct<-VHFlong.male$proportion
  VHFlong.female <- data.frame(VHFoverall[VHFoverall$gender==2 & is.finite(VHFoverall$gender),])
  VHFlong.female$female_pct<-VHFlong.female$proportion
  VHFlong.male<-VHFlong.male[,c("Symptom","male_pct")]
  VHFlong.female<-VHFlong.female[, c("Symptom","female_pct")]
  VHFlong_gender<-merge(VHFlong.male, VHFlong.female, by='Symptom', all.x=TRUE)
  
  graph1<-ggplot(data=VHFlong_gender, aes(x=male_pct, y=female_pct))+
    geom_text(aes(label=Symptom)) + geom_point(color="blue")+
    geom_abline(yintercept=0, slope=1) + coord_equal()+
    theme_bw() + scale_x_continuous (name="Percent of males with symptom")+
    scale_y_continuous(name="Percent of females with symptom")+
    ggtitle(paste("Symptoms of Prob/Confirmed cases\nComparison by gender,",now()))
  return(graph1)
}



sx_ppv<-function(VHFlongdata=VHFcase_long){
  ppv_df<-data.frame(sx_name=as.character, case_sx=as.numeric(), notcase_sx=as.numeric(), case_ppv=as.numeric(), sx_prevalence=as.numeric())
  VHFlongdata <- VHFlongdata [ VHFlongdata$StatusReport == 2 & !is.na(VHFlongdata$StatusReport) & VHFlongdata$CaseStatus %in% c("Confirmed","Not a case"),]
  for (i in names(table(VHFlongdata$symptom))){
    VHFlongdata$value <- factor(VHFlongdata$value)
    df<-VHFlongdata[ VHFlongdata$symptom == i,]
    b<-table(df$value, df$CaseStatus)
    ppv_df<-rbind(ppv_df, cbind(sx_name=i,case_sx=b[2],notcase_sx=b[4],case_ppv=b[2]/(b[2]+b[4]), 
                                sx_prevalence = (b[2]+b[4])/(b[1]+b[2]+b[3]+b[4])))
    print(b)
    print(paste("PPV for",i,":",b[2],b[4], b[2]/(b[2]+b[4])))
  }
  ppv_df$sx_name<-  gsub("_r",replacement = "", x = ppv_df$sx_name)
  
  return(assign("ppvdf", ppv_df, envir=.GlobalEnv))
}


sx_ppv_graph<-function(df=ppvdf){
  require(ggplot2)
  require(lubridate)
  ggplot(data=ppvdf, aes(x=as.numeric(as.character(case_ppv)), y=as.numeric(as.character(sx_prevalence))))+
    geom_text(aes(label=sx_name), position="jitter")+theme_minimal(base_size=20)+
    scale_y_continuous(name="Prevalence of symptom")+
    scale_x_continuous(name="Case PPV of symptom")+
    ggtitle(paste("Positive Predictive Value of symptoms\n(confirmed vs not cases, alive when reported)\n",now()))
}


sx_frequency_report <- function(VHFlongdata=VHFcase_long, docname="symptoms.pdf"){
  require(ggplot2)
  require(dplyr)
  require(stringr)
  require(reshape2)
  require(tidyr)
  
  VHFcasesx<-  VHFlongdata %>%
    filter(StatusReport == 2) %>%
    filter(CaseStatus %in% c("Confirmed","Not a case")) %>%
    filter(total_initial_symptoms >0) %>%
    mutate(group= paste0(CaseStatus,".",symptom)) %>%
    group_by(group) %>%
    summarize(avg = mean(value), sum=sum(value))
  VHFcasesx <- data.frame(VHFcasesx)
  VHFcasesx$symptom <- sapply(strsplit(as.character(VHFcasesx$group), "\\."), "[[", 2)
  VHFcasesx$Symptom <- gsub("_r",replacement = "", x = VHFcasesx$symptom)
  VHFcasesx$Symptom = reorder(VHFcasesx$Symptom,VHFcasesx$avg)
  
  VHFcasesx$CaseStatus <- sapply(strsplit(as.character(VHFcasesx$group), "\\."), "[[", 1)
  numcases<-data.frame(table(VHFcase_recode$CaseStatus))
  #VHFcasesx<-merge(VHFcasesx, numcases, by.x="CaseStatus",by.y="Var1", all.x=TRUE)
  #VHFcasesx<- VHFcasesx[ VHFcasesx$CaseStatus %in% c("Confirmed","Not a case"),]
  
  g1<- ggplot(data=VHFcasesx, aes(x=Symptom, y=avg*100, group=CaseStatus, colour=CaseStatus))+
    #geom_bar(position="dodge")+
    geom_point(size=3)+scale_colour_brewer(type="qual", palette="Set1")+
    coord_flip()+theme_bw()+
    scale_y_continuous("Percent of Confirmed or Non-Cases", breaks=seq(0,100,10))+
    ggtitle(paste("Initial symptom frequency by Epi Case Status\n",now()))
  #scale_color_manual(values=c("darkred","dodgerblue"))
  
  VHFalive <-
    VHFcase_recode %>%
    filter(StatusReport == 2) %>%
    filter(CaseStatus %in% c("Confirmed","Not a case"))
  
  g2 <- ggplot(data=VHFalive, aes(x=factor(total_initial_symptoms), y=..count.., group=CaseStatus, fill=CaseStatus))+
    geom_histogram(position="stack", binwidth=1)+
    theme_bw()+
    scale_fill_brewer(type="qual", palette="Set1")+
    ggtitle(paste("Number of initial symptoms on CIF, by final case status\namong persons alive when CIF was completed,\n",now()))+
    scale_x_discrete(name="Total number of initial symptoms")+facet_wrap(~contact, ncol=1)
  
  g2b<- ggplot(data=VHFalive, aes(x=factor(total_initial_symptoms), y=..count.., group=CaseStatus, fill=CaseStatus))+
    geom_histogram(position="fill", binwidth=1)+
    theme_bw()+
    scale_fill_brewer(type="qual", palette="Set1")+
    ggtitle(paste("Number of initial symptoms on CIF, by final case status\namong persons alive when CIF was completed,\n",now()))+
    scale_x_discrete(name="Total number of initial symptoms")+facet_wrap(~contact, ncol=1)+
    scale_y_continuous(name="Proportion")
  
  sx_frequency_gender()
  g3<-sx_frequency_genderscatter()
  g4<-sx_frequency_overall()
  g4b<-sx_frequency_overall_complete()
  sx_ppv()
  g5 <- sx_ppv_graph()
  setwd("c:/vhfdata/results")
  pdf(file = docname)
  print(g4)
  print(g4b)
  print(g2)
  print(g2b)
  print(g3)
  print(g5)
  print(g1)
  dev.off()
}



VHFdaily <-function(VHFdata=VHFcase_recode, 
                    yesterday=""){
  VHFyesterday<-VHFdata[ VHFdata$cDateReport==mdy(yesterday),]
  VHFweek<-VHFdata[VHFdata$cDateReport > mdy(yesterday)-days(7) & !is.na(VHFdata$cDateReport),]
  print(table(VHFyesterday$DateReport, VHFyesterday$CaseStatus))
  print(VHFyesterday[,c("ID","CaseStatus","StatusReport","Surname")])
  VHconfirmed<-VHFdata[ ymd(VHFdata$dDateOutcome)==mdy(yesterday) & !is.na(VHFdata$dDateOutcome) & VHFdata$CaseStatus=="Confirmed",c("ID","CaseStatus", "StatusReport","Surname")]
  print(VHconfirmed)
  print(table(VHconfirmed$StatusReport))
  print(table(VHFweek$SCOnset, VHFweek$CaseStatus,useNA="always"))
  print(table(VHFweek$VillageOnset, VHFweek$CaseStatus,useNA="always"))
  
  #table(VHFweek$DateReport, VHFweek$CaseStatus)
  #table(VHFweek$CaseStatus)
}

#VHFdaily(yesterday="10-28-2014")

