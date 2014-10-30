#MOH Lab data qc

require(xlsx)
setwd("c:/BO_DATA/Lab data for compare/")
cdc12oct<-read.xlsx2(file = "CDC_Ebola Lab results_12Oct14_Kenema _CDC_WHO.xlsx",sheetName = "Daily", stringsAsFactors=FALSE)
cdc13oct<-read.xlsx2(file = "CDC_Ebola Lab results_13Oct14_Kenema _CDC_WHO.xlsx",sheetName = "Daily", stringsAsFactors=FALSE)
cdc14oct<-read.xlsx2(file = "CDC_Ebola Lab results_14Oct14_Kenema _CDC_WHO.xlsx",sheetName = "Daily", stringsAsFactors=FALSE)
cdc15oct<-read.xlsx2(file = "CDC_Ebola Lab results_15Oct14_Kenema _CDC_WHO.xlsx",sheetName = "Daily", stringsAsFactors=FALSE)
cdc16oct<-read.xlsx2(file = "CDC_Ebola Lab results_16Oct14_Kenema _CDC_WHO.xlsx",sheetName = "Daily", StringsAsFactors=FALSE)
cdc17oct<-read.xlsx2(file = "Ebola Lab results_17Oct14_Kenema _CDC_WHO.xlsx",sheetName = "Daily", StringsAsFactors=FALSE)

removeblanks<-function(df){
  names(df)<-tolower(names(df))
  df<-df[ !(df$name == "") & !(df$specimen.type==""),]
}


cdc12oct<-removeblanks(cdc12oct)
cdc13oct<-removeblanks(cdc13oct)
cdc14oct<-removeblanks(cdc14oct)
cdc15oct<-removeblanks(cdc15oct)
cdc16oct<-removeblanks(cdc16oct)
cdc17oct<-removeblanks(cdc17oct)


moh12oct<-read.csv("MOH_CDC_12OCT2014.csv", head=TRUE, stringsAsFactors=FALSE)
moh13oct<-read.csv("MOH_CDC_13OCT2014.csv", head=TRUE, stringsAsFactors=FALSE)
moh14oct<-read.csv("MOH_CDC_14OCT2014.csv", head=TRUE, stringsAsFactors=FALSE)
moh15oct<-read.csv("MOH_CDC_15OCT2014.csv", head=TRUE, stringsAsFactors=FALSE)
moh16oct<-read.csv("MOH_CDC_16OCT2014.csv", head=TRUE, stringsAsFactors=FALSE)
moh17oct<-read.csv("MOH_CDC_17OCT2014.csv", head=TRUE, stringsAsFactors=FALSE)


moh12oct<-removeblanks(moh12oct)
moh13oct<-removeblanks(moh13oct)
moh14oct<-removeblanks(moh14oct)
moh15oct<-removeblanks(moh15oct)
moh16oct<-removeblanks(moh16oct)
moh17oct<-removeblanks(moh17oct)


missing_labs<-function(cdc,moh){
  print("CDC labs missing from MOH")
  print(cdc[ !(cdc$name %in% moh$name),c("name","district.of.origin","acute...convalescent...not.a.case")])
  print("MOH labs missing from CDC")
  print(  moh[ !(moh$name %in% cdc$name),c("name","district.of.origin","acute...convalescent...not.a.case")])
}



different_result <- function(cdc,moh){
  cdc$uid<-paste0(cdc$name, cdc$age)
  moh$uid<-paste0(moh$name, moh$age)
  mer<-merge(cdc, moh, by="uid", all.x=TRUE)
  mer$acute.x <- mer$acute...convalescent...not.a.case.x
  mer$acute.y <- mer$acute...convalescent...not.a.case.y
  check_case_agree <- dim(mer[ !(mer$acute.x == mer$acute.y) & is.finite(mer$acute.x) & is.finite(mer$acute.y) ,])
  print(check_case_agree)
  if (check_case_agree[1] >0) {
    print(mer[ !(mer$acute.x == mer$acute.y) & is.finite(mer$acute.x) & is.finite(mer$acute.y) ,])
  }
  check_confirmed_agree <- mer[ !(mer$confirmed...yes.no.y == mer$confirmed...yes.no.x) & is.finite(mer$confirmed..yes.no.y) & is.finite(mer$confirmed..yes.no.x),]
  print(dim(check_confirmed_agree))
  if (dim(check_confirmed_agree)[1] >0) {
  print(check_confirmed_agree)
  }
  }

missing_labs(cdc12oct, moh12oct)
missing_labs(cdc13oct, moh13oct)
missing_labs(cdc14oct, moh14oct)
missing_labs(cdc15oct, moh15oct)
missing_labs(cdc16oct, moh16oct)
missing_labs(cdc17oct, moh17oct)

different_result(cdc12oct,moh12oct)
different_result(cdc13oct,moh13oct)
different_result(cdc14oct,moh14oct)
different_result(cdc15oct,moh15oct)
different_result(cdc16oct,moh16oct)
different_result(cdc17oct,moh17oct)

cdc_complete<-read.xlsx2(file = "Ebola Lab results_17Oct14_Kenema _CDC_WHO.xlsx",sheetName = "complete list", StringsAsFactors=FALSE)
cdc_complete$Date.tested_fix<-as.Date(cdc_complete$Date.tested, origin = "1899-12-30")
cdc_complete[ tolower(cdc_complete$Village.Town)==tolower("tikonko"),]
cdc_complete[ tolower(cdc_complete$Village.Town)==tolower("tinkonko"),]
cdc_complete[ tolower(cdc_complete$Village.Town)==tolower("tinkono"),]
cdc_complete[ tolower(cdc_complete$Village.Town)==tolower("tinkoko"),]

