# 구 인기대출도서의 '도서명'과 '대출건수' 추출

name = c()
#name<-as.data.frame(name)
for (idx in 1:12) {
  temp = read.csv(paste0('C:/Users/student/R_semi_project/2018년_성북구_영유아(0~5)_대출목록/인기대출도서_2018-',idx,'.csv'), 
                 skip=13, header=T, stringsAsFactors=F)
  temp = temp$서명
  print(length(name))
  name = c(name, temp)
  print(length(name))
  name = unique(name)
  print(length(name))
}


######################################
# 성북구 영유아(0~5) 인기대출도서(1~12월) 병합(도서명, 대출건수) [1년 동안 대출 건수 확보]
temp = read.csv(paste0('C:/Users/student/R_semi_project/2018년_성북구_영유아(0~5)_대출목록/인기대출도서_2018-','1','.csv'), 
               skip=13, header=T, stringsAsFactors=F)
book = temp %>% select(서명, 대출건수)
book$대출건수 = as.numeric(book$대출건수)
for (idx in 2:12){
  temp<-read.csv(paste0('C:/Users/student/R_semi_project/2018년_성북구_영유아(0~5)_대출목록/인기대출도서_2018-',idx,'.csv'), 
                 skip=13, header=T, stringsAsFactors=F)
  temp_tp = temp %>% select(서명, 대출건수)
  for (i in 1:length(temp_tp$서명)) {
    if (temp_tp$서명[i] %in% book$서명){
      # book$대출건수[book$서명==temp_tp$서명[i]] = as.numeric(book$대출건수[book$서명==temp_tp$서명[i]])
      book$대출건수[book$서명==temp_tp$서명[i]] = as.numeric(book$대출건수[book$서명==temp_tp$서명[i]]) +
        temp_tp$대출건수[i]
    }
    else{
      book[length(book$서명)+1,] = c(temp_tp$서명[i], temp_tp$대출건수[i])
    }
  }
}

View(book)
######################################
# 1. 각 도서관 월마다 인기대출도서에 포함된 도서의 대출건수 추출하기
gu <- '성북구'
src_dir <- paste(gu ,'대출목록')
src_file_vec <- list.files(src_dir)
src_file_cnt <- length(src_file_vec)

# XX구 파일목록에서, 2018년 파일만 담은 목록 생성
file_Y2018_vec <- c()
for (item in src_file_vec) {
  if (grepl('2018', item)) {
    file_Y2018_vec <- c(file_Y2018_vec, item)
  }
}
file_Y2018_cnt <- length(file_Y2018_vec)


# 도서관 이름 목록 벡터 생성
gu_lib_vec <- c()
for (item in file_Y2018_vec) {
  if ( grepl(' 도서 ', item) ) {
    end_p <- regexpr(' 도서 ', item)
    lib_name <- substr(item, 1,end_p - 1)
  }
  else if ( grepl(' 장서', item) ) {
    end_p <- regexpr(' 장서 ', item)
    lib_name <- substr(item, 1,end_p - 1)
  }
  else {
    print('====exception!!!====')
    print(item)
    lib_name <- item
  }
  gu_lib_vec <- c(gu_lib_vec, lib_name)
}
gu_lib_vec <- unique(gu_lib_vec)
gu_lib_vec

# 최종 - 성북구 도서관별 대출 데이터 리스트 생성
library(readxl)

print('generating list...')
gu_lib_list <- list()
for (tmp_lib in gu_lib_vec) {
  
  tmp_list <- list()
  for (tmp_file in file_Y2018_vec) {
    tmp_file_head <- substr(tmp_file, 1, nchar(tmp_lib))
    
    if (tmp_file_head == tmp_lib) {
      temp_df <- 
        read_excel(paste0(src_dir, "/", tmp_file)) %>% 
        select(도서명,대출건수)
      temp_df <-
        temp_df %>% filter(temp_df$도서명 %in% book$서명)
      
      temp_df$대출건수 <- as.numeric(temp_df$대출건수)
      
      tmp_name <- substr( tmp_file,
                          regexpr('[(]', tmp_file)+1, 
                          regexpr('[)]', tmp_file)-1 )
      tmp_list[[tmp_name]] <- temp_df
    }
  }
  
  gu_lib_list[[tmp_lib]] <- tmp_list
}

# 하나의 도서관에서 중복된 도서의 대출건수 병합


tmp_list = gu_lib_list$달빛마루도서관
res_list = list()

cnt = 0
for (tmp_df in tmp_list) {
  cnt = cnt+1
  cat( length(tmp_df$도서명), '개에서', length(unique(tmp_df$도서명)), '개로 변환\n' )
  
  uniq_books = unique(tmp_df$도서명)
  uniq_cnt = c()
  for (idx in 1:length(uniq_books)) {
    tmp_book = uniq_books[idx]
    loan_num_vec = tmp_df$대출건수[tmp_df$도서명 == tmp_book]
    uniq_cnt[ idx ] = sum( loan_num_vec )
  }
  
  tmp_res_df = data.frame( 도서명 = uniq_books, 대출건수 = uniq_cnt )
  cat( length(tmp_res_df$도서명) == length(unique(tmp_res_df$도서명)), '\n' )  # TRUE이면 중복 없음
  
  res_list[[ names(tmp_list)[cnt] ]] = tmp_res_df
}
str(res_list)

###############################################
## 월별 누적 대출건수 제거

res_list_new = list()
cnt = 1
for (i in 1:11) {
  cnt = cnt+1
  temp_list_2 = res_list[[i+1]]
  temp_list_1 = res_list[[i]]
  
  for (idx in 1:length(temp_list_2$대출건수)) {
    if (temp_list_2$도서명[idx] %in% temp_list_1$도서명) {
      temp_list_2$도서명 = as.vector(temp_list_2$도서명)
      res_list[[i]]$도서명 = as.vector(res_list[[i]]$도서명)
      temp_list_2$대출건수[idx] = 
        temp_list_2$대출건수[idx] -
        res_list[[i]]$대출건수[res_list[[i]]$도서명==temp_list_2$도서명[idx]]
      
    }
  }
  
  tmp_res_df = data.frame( 도서명 = temp_list_2$도서명, 대출건수 = temp_list_2$대출건수 )
  
  res_list_new[[ names(tmp_list)[cnt] ]] = tmp_res_df
}


str(res_list_new)

# 달빛마루도서관의 누적 대출건수 문제를 해소한 2018년 자료

sb_lib_list2 = list()
num=0
for (lib_tmp in 1:length(res_list_all)) {
  # 성북구 내 도서관 1개 변수에 할당
  bf <- res_list_all[[lib_tmp]]
  num=num+1
  
  
  res_list_new = list()
  cnt = 1
  for (i in 1:11) {
    cnt = cnt + 1
    temp_list_2 = bf[[i+1]]
    temp_list_1 = bf[[i]]
    
    for (idx in 1:length(temp_list_2$대출건수)) {
      if (temp_list_2$도서명[idx] %in% temp_list_1$도서명) {
        temp_list_2$도서명<-as.vector(temp_list_2$도서명)
        bf[[i]]$도서명<-as.vector(bf[[i]]$도서명)
        temp_list_2$대출건수[idx] = 
          temp_list_2$대출건수[idx] -
          bf[[i]]$대출건수[bf[[i]]$도서명==temp_list_2$도서명[idx]]
        
      }
    }
    
    tmp_res_df = data.frame( 도서명 = temp_list_2$도서명, 대출건수 = temp_list_2$대출건수 )
    
    res_list_new[[ names(bf)[cnt] ]] = tmp_res_df
  }
  sb_lib_list2[[ names(res_list_all)[num] ]] = res_list_new
}

# saveRDS(sb_lib_list2, file = 'sb_lib_list2.rds')