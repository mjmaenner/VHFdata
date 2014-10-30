Lab questions:
  why did MOH drop 7 observations from CDC data- 
  different lab status in daily vs in complete the next day.

library(party)
treeresult<-ctree(VHFcase_recode[ VHFcase_recode$CaseStatus %in% c("Confirmed","Not a case"),],
      formula= factor(CaseStatus) ~ total_initial_symptoms+Fever_r+Fatigue_r+Anorexia_r+Headache_r+
                JointPain_r+Vomiting_r+Diarrhea_r+MusclePain_r+AbdPain_r+ChestPain_r+Cough_r+
                DiffBreathe_r+Conjunctivitis_r+Gender+Age)

library(randomForest)
vhcomplete<-VHFcase_recode[ VHFcase_recode$CaseStatus %in% c("Confirmed","Not a case") & !(is.na(VHFcase_recode$CaseStatus)),]
vhcomplete$case<-factor(vhcomplete$CaseStatus)
vhcomplete<-vhcomplete[ !(is.na(vhcomplete$Gender)) & !(is.na(vhcomplete$Age)),]
vhcomplete<-vhcomplete[, c("case","Fever_r","total_initial_symptoms","Fatigue_r","Anorexia_r","Headache_r","JointPain_r",
                           "Vomiting_r","Diarrhea_r","MusclePain_r","AbdPain_r","ChestPain_r","Cough_r","DiffBreathe_r","Conjunctivitis_r",
                           "Gender","Age")]
                     
rf<-randomForest(x=vhcomplete[,-1], y=vhcomplete$case, importance=TRUE,ntree=2000)

ggplot(data=VHFcase_recode, aes(x=total_initial_symptoms, fill=CaseStatus, y=..count..))+geom_bar()
