labs<-read.csv("c:/vhfdata/labs2nov.csv", stringsAsFactors=TRUE)
labs$id<-tolower(labs$MSF.or.other.district..)
labs$id<-gsub("-", "", labs$id)
labs$id<-gsub(" ", "", labs$id)

library(ggplot2)
library(lubridate)
library(stringr)

labs$date_test<-dmy(labs$Date.tested)
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
  ggtitle("Samples processed by CDC Lab, Sierra Leone, Aug - Nov 1, 2014\ngrouped by status (alive/dead) and result")


g1a<-ggplot(data=labs, aes(x=as.Date(date_test), fill=type))+
  geom_histogram(binwidth=1)+theme_bw()+
  scale_fill_brewer(type="qual", palette="Set1")+
  scale_x_date(name="Date sample tested (by week)")+
  ggtitle("Daily samples processed by CDC Lab, Sierra Leone, Aug - Nov 1, 2014\nby status (alive/dead)")




g2<-ggplot(data=labs, aes(x=as.Date(date_test), fill=case_status))+
  geom_histogram(binwidth=7, position="fill")+theme_bw()+
  scale_fill_brewer(type="qual", palette="Set1")+facet_wrap(~type, ncol=1)+
  scale_x_date(name="Date sample tested (by week)")+
  scale_y_continuous(name="Proportion of labs processed each week")+
  ggtitle("Proportion of samples processed by CDC Lab, Sierra Leone, Aug - Nov 1, 2014\ngrouped by status (alive/dead) and result")

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
labs$district<-ifelse(labs$d %in% c("freetown","lumley","mountaincut","wa","war","waterloo","wellington","western","westernarea","westernruler","westernrural","westernurban","wr","wru","wrural","wur","wurban","wurban"), "western",labs$district)
labs$district<-ifelse(labs$d %in% c("","fobey","southern"),"blank/unknown", labs$district)

g3<-ggplot(data=labs, aes(x=as.Date(date_test), fill=type))+
  geom_histogram(binwidth=7)+
  theme_bw()+facet_wrap(~district)+
  scale_fill_brewer(type="qual", palette="Set2")+
  ggtitle("Samples tested at CDC Lab/nby week and district of origin")
