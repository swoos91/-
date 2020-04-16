library(dplyr)
library(readxl)

# 성북구 파일목록 확보
src_dir <- c('C:/Users/student/Downloads/성북구 대출목록')
src_file_vec <- list.files(src_dir)
src_file_cnt <- length(src_file_vec)

substr('123',1,2) # '12'
substr('성북구 도서장 목록', 1, 5)
regexpr(' 도서 ','성북구도서관 도서 목록')
grep('a', c('a', 'b', 'a'))


# 성북구 파일목록에서, 2018년 파일만 담은 목록 생성
file_Y2018_vec <- c()
for (item in src_file_vec) {
  if (grepl('2018', item)) {
    file_Y2018_vec <- c(file_Y2018_vec, item)
  }
}
file_Y2018_cnt <- length(file_Y2018_vec)


# 도서관 이름 목록 벡터 생성
seongbook_lib_vec <- c()
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
  seongbook_lib_vec <- c(seongbook_lib_vec, lib_name)
}
seongbook_lib_vec <- unique(seongbook_lib_vec)
seongbook_lib_vec


# 최종 - 성북구 도서관별 대출 데이터 리스트 생성
sb_lib_list <- list()
for (tmp_lib in seongbook_lib_vec) {
  
  tmp_list <- list()
  for (tmp_file in file_Y2018_vec) {
    tmp_file_head <- substr(tmp_file, 1, nchar(tmp_lib))
    
    if (tmp_file_head == tmp_lib) {
      temp_df <- 
        read_excel(paste0(src_dir, "/", tmp_file)) %>% 
        select(도서명,대출건수)
      temp_df$대출건수 <- as.numeric(temp_df$대출건수)
      
      tmp_name <- substr( tmp_file,
                          regexpr('[(]', tmp_file)+1, 
                          regexpr('[)]', tmp_file)-1 )
      tmp_list[[tmp_name]] <- temp_df
    }
  }
  
  sb_lib_list[[tmp_lib]] <- tmp_list
}

View(sb_lib_list)

# save(sb_lib_list, file = '2018년 성북구 도서관별 대출목록.RData')
# rm(sb_lib_list)
# load('C:/Users/student/Desktop/sam/팀 프로젝트/2018년 성북구 도서관별 대출목록.RData')
# str(sb_lib_list)
