#labs<-read.csv("c:/vhfdata/lab2nov.csv", stringsAsFactors=FALSE)

library(xlsx)
labs<-read.xlsx2("c:/vhfdata/Ebola Lab results_06NOV14_Kenema _CDC_WHO.xlsx",sheetName = "complete list", stringsAsFactors=FALSE)

labs$id<-tolower(labs$MSF.or.other.district..)
labs$id<-gsub("-", "", labs$id)
labs$id<-gsub(" ", "", labs$id)

library(ggplot2)
library(lubridate)
library(stringr)

labs$date_test_1 <- as.Date(dmy(labs$Date.tested))
labs$date_test_excel <- as.Date(as.numeric(as.character(labs$Date.tested)),origin="1899-12-30")
labs$date_tested <- ymd(ifelse(is.na(labs$date_test_1), as.character(labs$date_test_excel), as.character(labs$date_test_1)))

labs$date_onset_1 <- dmy(labs$Date.of.symptom.onset)
labs$date_onset_excel <- as.Date(as.numeric(as.character(labs$Date.of.symptom.onset)),origin="1899-12-30")
labs$date_onset <- ymd(ifelse(is.na(labs$date_onset_1), as.character(labs$date_onset_excel), as.character(labs$date_onset_1)))



table(labs$specimen.type, useNA="always")


labs$type<-ifelse(str_detect(tolower(as.character(labs$specimen.type)), "blood"), "alive",
                  ifelse(str_detect(tolower(as.character(labs$specimen.type)), "swab"), "corpse", "other/unknown"))
labs$type<-ifelse( tolower(as.character(labs$specimen.type)) %in%
                     c("heart blood", "heart blood?", "blood/corpse", "blood/swab", "blood swab", "blood (heart ?)"),
                   "other/unknown", labs$type)

labs$case_status<-ifelse(tolower(as.character(labs$Acute...convalescent...not.a.case)) %in% c("acute", "acute case"), "acute",
                         ifelse(str_detect(tolower(as.character(labs$Acute...convalescent...not.a.case)), "not"), "not a case",
                                ifelse(str_detect(tolower(as.character(labs$Acute...convalescent...not.a.case)), "pending"), "pending",
                                       ifelse(str_detect(tolower(as.character(labs$Acute...convalescent...not.a.case)), "convalescent"), "convalescent","other"))))

table(labs$type, labs$specimen.type)

g1<-ggplot(data=labs, aes(x=as.Date(date_tested), fill=case_status))+
  geom_histogram(binwidth=7)+theme_bw()+
  scale_fill_brewer(type="qual", palette="Set1")+facet_wrap(~type, ncol=1)+
  scale_x_date(name="Date sample tested (by week)")+
  ggtitle(paste("Samples tested by CDC Lab, Sierra Leone, Aug - Nov 2, 2014\ngrouped by status (alive/corpse)\n",now()))


g1a<-ggplot(data=labs, aes(x=as.Date(date_tested), fill=type))+
  geom_histogram(binwidth=1)+theme_bw()+
  scale_fill_brewer(type="qual", palette="Dark2")+
  scale_x_date(name="Date sample tested (by week)")+
  ggtitle(paste("Daily samples tested by CDC Lab, Sierra Leone, Aug - Nov, 2014\nby status (alive/corpse)\n", now()))




g2<-ggplot(data=labs, aes(x=as.Date(date_tested), fill=case_status))+
  geom_histogram(binwidth=7, position="fill")+theme_bw()+
  scale_fill_brewer(type="qual", palette="Set1")+facet_wrap(~type, ncol=1)+
  scale_x_date(name="Date sample tested (by week)")+
  scale_y_continuous(name="Proportion of labs tested each week")+
  ggtitle(paste("Proportion of samples tested by CDC Lab, Sierra Leone, Aug - Nov, 2014\ngrouped by status (alive/dead) and result\n",now()))

labs$d<-tolower(as.character(labs$District.of.Origin))
labs$d <- gsub("\\?", "", labs$d)
labs$d <- gsub(" ", "", labs$d)
labs$d <- gsub("/", "", labs$d)

table(labs$d, useNA="always")
labs$district <- labs$d
labs$district<-ifelse(labs$d == "bo", "bo", labs$district)
labs$district<-ifelse(labs$d == "bombali", "bombali", labs$district)
labs$district<-ifelse(labs$d == "bonthe", "bonthe", labs$district)
labs$district<-ifelse(labs$d == "kailahun", "kailahun", labs$district)
labs$district<-ifelse(labs$d == "kambia", "kambia", labs$district)
labs$district<-ifelse(labs$d %in% c( "kenema", "tongo-kenema"), "kenema", labs$district)
labs$district<-ifelse(labs$d == "koinadugu", "koinodugu", labs$district)
labs$district<-ifelse(labs$d == "kono", "kono", labs$district)
labs$district<-ifelse(labs$d %in% c("kaiyamba","moyamba"), "moyamba", labs$district)
labs$district<-ifelse(labs$d == "portloko", "portloko", labs$district)
labs$district<-ifelse(labs$d == "pujehun" , "pujehun", labs$district)
labs$district<-ifelse(labs$d %in% c("tonkolili","tonkokili"), "tonkolili", labs$district)
labs$district<-ifelse(labs$d %in% c("freetown","lumley","mountaincut","wa","war","waterloo","wellington","western","westernarea","westernruler","westernrural","westernurban","wr","wru","wrural","wur","wurban","wurban","wu"), "western",labs$district)
labs$district<-ifelse(labs$d %in% c("","fobey","southern"),"blank/unknown", labs$district)

lab_bo<-labs[ labs$district=="bo",]

g3<-ggplot(data=labs, aes(x=as.Date(date_tested), fill=type))+
  geom_histogram(binwidth=7)+
  theme_bw()+facet_wrap(~district)+
  scale_fill_brewer(type="qual", palette="Dark2")+
  ggtitle(paste("Samples tested at CDC Lab by week and district of origin\n",now()))

g1_bo<-ggplot(data=lab_bo, aes(x=as.Date(date_test), fill=case_status))+
  geom_histogram(binwidth=7)+theme_bw()+
  scale_fill_brewer(type="qual", palette="Set1")+facet_wrap(~type, ncol=1)+
  scale_x_date(name="Date sample tested (by week)")+
  ggtitle(paste("Samples tested by CDC Lab, Bo District, Aug - Nov, 2014\ngrouped by status (alive/corpse)\n",now()))

corpse_df<-data.frame(with(lab_bo, table(case_status, month(as.Date(date_test)), type, useNA="always")))
corpse_df<-corpse_df[ corpse_df$case_status %in% c("acute","not a case") & !is.na(corpse_df$case_status),]
corpse_df<-corpse_df[ !is.na(corpse_df$Var2 ) & corpse_df$type %in% c("alive","corpse"),]

write.csv(corpse_df, "c:/vhfdata/corpses.csv", row.names=FALSE)

VHFconfirm<-VHFcase_recode[ VHFcase_recode$CaseStatus == "Confirmed" & !is.na(VHFcase_recode$CaseStatus),]
VHFconfirm$deathstatus<-with(VHFconfirm, ifelse(StatusReport==1 & !is.na(StatusReport), "corpse", 
                                         ifelse(StatusAsOfCurrentDate=="Dead", "died after CIF", 
                                         ifelse(StatusAsOfCurrentDate=="Alive", "Alive or Unknown",              
                                                       "Alive or Unknown"))))

VHFconfirm$eventmonth<-ifelse(!is.na(month(VHFconfirm$dDateReport)), month(VHFconfirm$dDateReport),
                              ifelse(!is.na(month(VHFconfirm$dDateOnset)), month(VHFconfirm$dDateOnset), 
                                     month(VHFconfirm$dDateOutcome)))


write.csv(file="c:/vhfdata/deathstatusconfirmed.csv", as.matrix(with(VHFconfirm, table(eventmonth, deathstatus, useNA="always"))))

g1a_bo<-ggplot(data=lab_bo, aes(x=as.Date(date_test), fill=type))+
  geom_histogram(binwidth=1)+theme_bw()+
  scale_fill_brewer(type="qual", palette="Dark2")+
  scale_x_date(name="Date sample tested (by week)")+
  ggtitle(paste("Daily samples tested by CDC Lab, Bo District, Aug - Nov, 2014\nby status (alive/corpse)\n", now()))




g2_bo<-ggplot(data=lab_bo, aes(x=as.Date(date_test), fill=case_status))+
  geom_histogram(binwidth=7, position="fill")+theme_bw()+
  scale_fill_brewer(type="qual", palette="Set1")+facet_wrap(~type, ncol=1)+
  scale_x_date(name="Date sample tested (by week)")+
  scale_y_continuous(name="Proportion of labs tested each week")+
  ggtitle(paste("Proportion of samples tested by CDC Lab, Bo District, Aug - Nov, 2014\ngrouped by status (alive/dead) and result\n",now()))


g3_bo<-ggplot(data=lab_bo, aes(x=as.Date(date_test), fill=type))+
  geom_histogram(binwidth=7)+
  theme_bw()+facet_wrap(~district)+
  scale_fill_brewer(type="qual", palette="Dark2")+
  ggtitle(paste("Samples tested at CDC Lab by week and type\n",now()))






pdf("c:/vhfdata/results/labs-bo.pdf",width = 8, height=8)
print(g1_bo)
print(g2_bo)
print(g1a_bo)
print(g3_bo)
dev.off()

pdf("c:/vhfdata/results/cdc_lab.pdf",width = 8, height=8)
  print(g1)
  print(g2)
  print(g1a)
  print(g3)
#  print(g5)
#  print(g6) removed at request of lab
dev.off()
