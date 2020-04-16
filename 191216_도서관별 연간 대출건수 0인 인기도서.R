setwd('c:/semi/6조')
load('2018년 성북구 도서관별 대출목록.Rdata')


# '영유아 인기도서 리스트'와 '1월 도서 대출 목록'에서 겹치는 도서를 추출

name<-c()
#name<-as.data.frame(name)
for (idx in 1:12) {
  temp<-read.csv(paste0('c:\\semi\\6조\\성북구\\인기대출도서_2018-',idx,'.csv'), 
                 skip=13, header=T, stringsAsFactors=F)
  temp<-temp$서명
  print(length(name))
  name<-c(name, temp)
  print(length(name))
  name<-unique(name)
  print(length(name))
}

lib<-sb_lib_list$성북정보도서관

lib1<-lib[[1]]
lib1_0<-lib1 %>% filter(대출건수==0)
ext<-c()
for (i in 1:length(lib1_0$도서명)) {
    if (lib1_0$도서명[i] %in% name) {
      print(lib1_0$도서명[i])
      ext<-c(lib1_0$도서명[i],ext)
    }
}



# '1월 도서 대출 목록'과 '12월 도서 대출 목록' 가운데 위에서 추출한 도서를 중심으로 대출 건수가 0인 도서를 재추출
lib12<-lib[[12]]
lib12_0<-lib12 %>% filter(대출건수==0)

ext1<-c()
for (i in 1:length(lib12_0$도서명)) {
    if (lib12_0$도서명[i] %in% ext) {
      print(lib12_0$도서명[i])
      ext1<-c(lib12_0$도서명[i],ext1)
    }
}