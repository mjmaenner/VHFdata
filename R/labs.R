labs<-read.csv("c:/vhfdata/lab2nov.csv", stringsAsFactors=FALSE)
labs$id<-tolower(labs$MSF.or.other.district..)
labs$id<-gsub("-", "", labs$id)
labs$id<-gsub(" ", "", labs$id)

library(ggplot2)
library(lubridate)
library(stringr)

labs$date_test<-dmy(labs$Date.tested)
labs$date_onset<-dmy(labs$Date.of.symptom.onset)
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

g1<-ggplot(data=labs, aes(x=as.Date(date_test), fill=case_status))+
  geom_histogram(binwidth=7)+theme_bw()+
  scale_fill_brewer(type="qual", palette="Set1")+facet_wrap(~type, ncol=1)+
  scale_x_date(name="Date sample tested (by week)")+
  ggtitle(paste("Samples tested by CDC Lab, Sierra Leone, Aug - Nov 2, 2014\ngrouped by status (alive/corpse)\n",now()))


g1a<-ggplot(data=labs, aes(x=as.Date(date_test), fill=type))+
  geom_histogram(binwidth=1)+theme_bw()+
  scale_fill_brewer(type="qual", palette="Dark2")+
  scale_x_date(name="Date sample tested (by week)")+
  ggtitle(paste("Daily samples tested by CDC Lab, Sierra Leone, Aug - Nov, 2014\nby status (alive/corpse)\n", now()))




g2<-ggplot(data=labs, aes(x=as.Date(date_test), fill=case_status))+
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

g3<-ggplot(data=labs, aes(x=as.Date(date_test), fill=type))+
  geom_histogram(binwidth=7)+
  theme_bw()+facet_wrap(~district)+
  scale_fill_brewer(type="qual", palette="Dark2")+
  ggtitle(paste("Samples tested at CDC Lab by week and district of origin\n",now()))

g1_bo<-ggplot(data=lab_bo, aes(x=as.Date(date_test), fill=case_status))+
  geom_histogram(binwidth=7)+theme_bw()+
  scale_fill_brewer(type="qual", palette="Set1")+facet_wrap(~type, ncol=1)+
  scale_x_date(name="Date sample tested (by week)")+
  ggtitle(paste("Samples tested by CDC Lab, Bo District, Aug - Nov, 2014\ngrouped by status (alive/corpse)\n",now()))


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


labacute<-labs[ labs$case_status %in% c("acute"),]
labacute<-labacute[ !is.na(labacute$date_onset),]
labacute$days_from_onset <- interval(start = labacute$date_onset, end=labacute$date_test)/ddays(1)
labacute$pcr_i<-labacute$qRT.PCR.I..NP.
labacute$pcr_i<-ifelse(labacute$pcr_i=="26/25", "25.5",
                       ifelse(labacute$pcr_i=="32/31", "31.5",
                       ifelse(labacute$pcr_i=="`16", "16",
                       ifelse(labacute$pcr_i=="neg", "45", labacute$pcr_i))))

labacute$pcr_ii<-labacute$qRT.PCR.II..VP40.
labacute$pcr_ii<-ifelse(labacute$pcr_ii=="24/23", "23.5",
                       ifelse(labacute$pcr_ii=="30/29", "29.5",
                       ifelse(labacute$pcr_ii=="neg", "45", labacute$pcr_ii)))

labacute$concordance <- ifelse(labacute$pcr_i == 45 | labacute$pcr_ii == 45, "discordant","concordant")

g4<-ggplot(data=labacute, aes(x=days_from_onset, y=as.numeric(as.character(pcr_i))))+
    geom_jitter(alpha=0.2)+scale_x_continuous(limits=c(0,20))+theme_bw()+
    geom_smooth(method="loess")+
    scale_y_continuous(name="PCR I", breaks=seq(0,45,5), labels=c(seq(0,40,5),"Neg"))+
    ggtitle("PCR I test value among confirmed cases\ncompared to days from symptom onset")

g5<-ggplot(data=labacute, aes(x=days_from_onset, y=as.numeric(as.character(pcr_ii))))+
  geom_jitter(alpha=0.2)+scale_x_continuous(limits=c(0,20))+theme_bw()+
  geom_smooth(method="loess")+
  scale_y_continuous(name="PCR II", breaks=seq(0,45,5), labels=c(seq(0,40,5),"Neg"))+
  ggtitle("PCR II test value among confirmed cases\ncompared to days from reported symptom onset")


g6<-
  ggplot(data=labacute, aes(x=as.numeric(as.character(pcr_i)), y=as.numeric(as.character(pcr_ii)),color=concordance))+
  geom_jitter(alpha=0.2)+theme_bw()+
  geom_abline(intercept=0, slope=1)+
  scale_color_manual(values=c("concordant"="black","discordant"="red"))+
  scale_y_continuous(name="PCR II", breaks=seq(0,45,5), labels=c(seq(0,40,5),"Neg"))+
  scale_x_continuous(name="PCR I", breaks=seq(0,45,5), labels=c(seq(0,40,5),"Neg"))+
  ggtitle(paste("Concordance between PCR I and PCR II test results\namong", 
                dim(labacute)[1], "Confirmed Ebola cases tested by CDC Lab "))+
  coord_equal()+
  guides(colour = guide_legend(override.aes = list(alpha = 1, size=6)))




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
  print(g5)
  print(g6)
dev.off()

g3_bo+facet_wrap(~Referred.from, ncol=1)
