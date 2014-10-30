VHFconf<-VHFcase_recode[ VHFcase_recode$CaseStatus == "Confirmed" & VHFcase_recode$StatusReport==2 & !is.na(VHFcase_recode$StatusReport),]
VHFconf$id<-gsub("SL ","", VHFconf$ID)
VHFconf$id<-gsub("-","", VHFconf$id)
VHFconf$id<-gsub(" ","", VHFconf$id)

msf<-read.csv("c:/vhfdata/msf.csv",head=TRUE, stringsAsFactors=FALSE)

VHFconf$MSFcapture<- VHFconf$id %in% msf$IdMSF

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