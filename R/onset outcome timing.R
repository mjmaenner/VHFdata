VHFconf<-VHFcase_recode[ VHFcase_recode$CaseStatus == "Confirmed" ,]
VHFconf<-VHFcase_recode
VHFconf$id<-gsub("SL ","", VHFconf$ID)
VHFconf$id<-gsub("-","", VHFconf$id)
VHFconf$id<-gsub(" ","", tolower(VHFconf$id))

msf<-read.csv("c:/vhfdata/msf1030.csv",head=TRUE, stringsAsFactors=FALSE)
msf$bo_id<-tolower(msf$bo_id)
msf_subset<-msf[ !(msf$bo_id %in% c("xx","")),]
msf_match<-merge(msf_subset, VHFconf, by.x="bo_id", by.y="id", all.x=TRUE)
msf_match[,c("bo_id", "ID")]
msf_match <- msf_match[ msf_match$FinalStatus.x=="Confirmed",]
msf_match[!(as.character(msf_match$CaseStatus) == as.character(msf_match$FinalStatus.x)) ,c("ID","bo_id", "CaseStatus", "FinalStatus.x","Surname","OtherNames")]
######

labs<-read.csv("c:/vhfdata/labs1nov.csv", head=TRUE, stringsAsFactors=FALSE)
labs$id<-tolower(as.character(labs$MSF.or.other.district..))
labs$id<-gsub("SL", "", labs$id)
labs$id<-gsub("-", "",labs$id)


table(labs$id %in% VHFconf$id)
vhflabs<-merge(VHFconf, labs, by="id", all.x=TRUE)
table(vhflabs$CaseStatus, vhflabs$Acute...convalescent...not.a.case, useNA="always")
vhflabs<-vhflabs[ (vhflabs$CaseStatus == "Confirmed" | vhflabs$Acute...convalescent...not.a.case=="acute") & !(is.na(vhflabs$Acute...convalescent...not.a.case)),]
vhflabs<-vhflabs[ !(vhflabs$CaseStatus == "Confirmed" | !vhflabs$Acute...convalescent...not.a.case=="acute"),]


VHFdeath<-VHFcase_recode[ VHFcase_recode$StatusReport ==1, ]
VHFdeath$Case_Status<-factor(VHFdeath$CaseStatus, levels = c("Confirmed","Probable","Suspect","Not a case"))
  
g1<-ggplot(VHFdeath, aes(x=dDateReport,  fill=Case_Status))+
  geom_histogram(binwidth=7)+
  scale_fill_manual(values=c("#e31a1c","#fb9a99","#b2df8a","#1f78b4")) +
  scale_x_date(name="Date CIF Created (DateReport)")+
  scale_y_continuous(name="Number of burials per week")+
  ggtitle(paste("Case status of weekly burials - Bo\n", now()))+
  theme_minimal()

g2<-ggplot(VHFdeath, aes(x=dDateReport,  fill=Case_Status))+
  geom_histogram(binwidth=7, position="fill")+
  scale_fill_manual(values=c("#e31a1c","#fb9a99","#b2df8a","#1f78b4")) +
  scale_y_continuous(name="Proportion of burials")+
  scale_x_date(name="Date CIF Created (DateReport)")+
  ggtitle(paste("Case status of weekly burials - Bo\n", now()))+
  theme_minimal()

pdf("c:/vhfdata/results/deathsovertime.pdf")
print(g1)
print(g2)
dev.off()

str_length(msf_match[1,"CaseStatus"])
str_length(msf_match[1,"FinalStatus.x"])


#outcomes with MSF
VHFconf$mdate<-mdy_hms(VHFconf$DateReport)
VHFconf[ !is.na(VHFconf$FinalStatus) & VHFconf$FinalStatus==2 & VHFconf$CaseStatus=="Confirmed",c("ID","MSFcapture", "Surname","DateReport", "dDateReport", "cDateReport","mdate")]
VHFcase_recode$mdate <- mdy_hms(VHFcase_recode$DateReport)

msf_match[!(as.character(msf_match$CaseStatus) == as.character(msf_match$FinalStatus.x)) ,c("ID","bo_id", "CaseStatus", "FinalStatus.x","Surname","OtherNames")]

as.character(msf_match[1,"CaseStatus"]) == as.character(msf_match[1,"FinalStatus.x"]) 


