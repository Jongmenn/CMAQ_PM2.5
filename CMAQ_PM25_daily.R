library(data.table)
library(dplyr)
library(lubridate)
#원시자료(시간)-> 일단위 가공한 자료 
setwd("D:\\EUMC\\데이터관리\\아주대PM2.5\\정리데이터\\PM25_daily_LSB")

#해당 파일만 
lf06<-list.files()[grep(2006,list.files())]
lf07<-list.files()[grep(2007,list.files())]
lf08<-list.files()[grep(2008,list.files())]
lf09<-list.files()[grep(2009,list.files())]
lf10<-list.files()[grep(2010,list.files())]
lf11<-list.files()[grep(2011,list.files())]
lf12<-list.files()[grep(2012,list.files())]
lf13<-list.files()[grep(2013,list.files())]
lf14<-list.files()[grep(2014,list.files())]
lf15<-list.files()[grep(2015,list.files())]


#자료 정리용 함수
#세종 제외 했을때/포함일때 연도별로 나눠서 이용

cmaq.func<-function(data){
  cmaq.list=NULL
  for(i in 1:12){
    
    d<-read.csv(data[i])
    d$sido=substr(d$SIGUNGU,1,2)
    d01<-subset(d,sido==11) %>% select(-c(SIGUNGU,sido)) %>%  apply(2,mean) %>% as.data.frame;d01$sido="서울"
    d02<-subset(d,sido==26) %>% select(-c(SIGUNGU,sido)) %>%  apply(2,mean) %>% as.data.frame;d02$sido="부산"
    d03<-subset(d,sido==27) %>% select(-c(SIGUNGU,sido)) %>%  apply(2,mean) %>% as.data.frame;d03$sido="대구" 
    d04<-subset(d,sido==28) %>% select(-c(SIGUNGU,sido)) %>%  apply(2,mean) %>% as.data.frame;d04$sido="인천"
    d05<-subset(d,sido==29) %>% select(-c(SIGUNGU,sido)) %>%  apply(2,mean) %>% as.data.frame;d05$sido="광주"
    d06<-subset(d,sido==30) %>% select(-c(SIGUNGU,sido)) %>%  apply(2,mean) %>% as.data.frame;d06$sido="대전"
    d07<-subset(d,sido==31) %>% select(-c(SIGUNGU,sido)) %>%  apply(2,mean) %>% as.data.frame;d07$sido="울산"
    d08<-subset(d,sido==41) %>% select(-c(SIGUNGU,sido)) %>%  apply(2,mean) %>% as.data.frame;d08$sido="경기"
    d09<-subset(d,sido==42) %>% select(-c(SIGUNGU,sido)) %>%  apply(2,mean) %>% as.data.frame;d09$sido="강원"
    d10<-subset(d,sido==43) %>% select(-c(SIGUNGU,sido)) %>%  apply(2,mean) %>% as.data.frame;d10$sido="충북"
    d11<-subset(d,sido==44) %>% select(-c(SIGUNGU,sido)) %>%  apply(2,mean) %>% as.data.frame;d11$sido="충남"
    d12<-subset(d,sido==45) %>% select(-c(SIGUNGU,sido)) %>%  apply(2,mean) %>% as.data.frame;d12$sido="전북"
    d13<-subset(d,sido==46) %>% select(-c(SIGUNGU,sido)) %>%  apply(2,mean) %>% as.data.frame;d13$sido="전남"
    d14<-subset(d,sido==47) %>% select(-c(SIGUNGU,sido)) %>%  apply(2,mean) %>% as.data.frame;d14$sido="경북"
    d15<-subset(d,sido==48) %>% select(-c(SIGUNGU,sido)) %>%  apply(2,mean) %>% as.data.frame;d15$sido="경남"
    d16<-subset(d,sido==49 |sido==50) %>% select(-c(SIGUNGU,sido)) %>%  apply(2,mean) %>% as.data.frame;d16$sido="제주"
    
    cmaq<-(rbind(d01,d02,d03,d04,d05,d06,d07,d08,d09,d10,d11,d12,d13,d14,d15,d16))
    names(cmaq)[1]="CMAQ_PM2.5"
    cmaq$date=ymd(substr(row.names(cmaq),2,9))
    cmaq.list[[i]]<-cmaq
  }
  rbindlist(cmaq.list)
}


cmaq.func2<-function(data){
  cmaq.list=NULL
  for(i in 1:12){
    
    d<-read.csv(data[i])
    d$sido=substr(d$SIGUNGU,1,2)
    d01<-subset(d,sido==11) %>% select(-c(SIGUNGU,sido)) %>%  apply(2,mean) %>% as.data.frame;d01$sido="서울"
    d02<-subset(d,sido==26) %>% select(-c(SIGUNGU,sido)) %>%  apply(2,mean) %>% as.data.frame;d02$sido="부산"
    d03<-subset(d,sido==27) %>% select(-c(SIGUNGU,sido)) %>%  apply(2,mean) %>% as.data.frame;d03$sido="대구" 
    d04<-subset(d,sido==28) %>% select(-c(SIGUNGU,sido)) %>%  apply(2,mean) %>% as.data.frame;d04$sido="인천"
    d05<-subset(d,sido==29) %>% select(-c(SIGUNGU,sido)) %>%  apply(2,mean) %>% as.data.frame;d05$sido="광주"
    d06<-subset(d,sido==30) %>% select(-c(SIGUNGU,sido)) %>%  apply(2,mean) %>% as.data.frame;d06$sido="대전"
    d07<-subset(d,sido==31) %>% select(-c(SIGUNGU,sido)) %>%  apply(2,mean) %>% as.data.frame;d07$sido="울산"
    d08<-subset(d,sido==36) %>% select(-c(SIGUNGU,sido)) %>%  apply(2,mean) %>% as.data.frame;d08$sido="세종"
    d09<-subset(d,sido==41) %>% select(-c(SIGUNGU,sido)) %>%  apply(2,mean) %>% as.data.frame;d09$sido="경기"
    d10<-subset(d,sido==42) %>% select(-c(SIGUNGU,sido)) %>%  apply(2,mean) %>% as.data.frame;d10$sido="강원"
    d11<-subset(d,sido==43) %>% select(-c(SIGUNGU,sido)) %>%  apply(2,mean) %>% as.data.frame;d11$sido="충북"
    d12<-subset(d,sido==44) %>% select(-c(SIGUNGU,sido)) %>%  apply(2,mean) %>% as.data.frame;d12$sido="충남"
    d13<-subset(d,sido==45) %>% select(-c(SIGUNGU,sido)) %>%  apply(2,mean) %>% as.data.frame;d13$sido="전북"
    d14<-subset(d,sido==46) %>% select(-c(SIGUNGU,sido)) %>%  apply(2,mean) %>% as.data.frame;d14$sido="전남"
    d15<-subset(d,sido==47) %>% select(-c(SIGUNGU,sido)) %>%  apply(2,mean) %>% as.data.frame;d15$sido="경북"
    d16<-subset(d,sido==48) %>% select(-c(SIGUNGU,sido)) %>%  apply(2,mean) %>% as.data.frame;d16$sido="경남"
    d17<-subset(d,sido==49 |sido==50) %>% select(-c(SIGUNGU,sido)) %>%  apply(2,mean) %>% as.data.frame;d17$sido="제주"
    
    cmaq<-(rbind(d01,d02,d03,d04,d05,d06,d07,d08,d09,d10,d11,d12,d13,d14,d15,d16,d17))
    names(cmaq)[1]="CMAQ_PM2.5"
    cmaq$date=ymd(substr(row.names(cmaq),2,9))
    cmaq.list[[i]]<-cmaq
  }
  rbindlist(cmaq.list)
}


cmaq06<-cmaq.func(lf06)
cmaq07<-cmaq.func(lf07)
cmaq08<-cmaq.func(lf08)
cmaq09<-cmaq.func(lf09)
cmaq10<-cmaq.func(lf10)
cmaq11<-cmaq.func(lf11)
cmaq12<-cmaq.func(lf12)
cmaq13<-cmaq.func2(lf13)
cmaq14<-cmaq.func2(lf14)
cmaq15<-cmaq.func2(lf15)

cmaq_m<-rbind(cmaq06,cmaq07,
              cmaq08,cmaq09,
              cmaq10,cmaq11,
              cmaq12,cmaq13,
              cmaq14,cmaq15)

# write.csv(cmaq_m,file="D:\\EUMC\\데이터관리\\아주대PM2.5\\정리데이터\\CMAQ_PM25_06_15.csv",row.names=F,na="")





