# setwd()
# rm(list = ls())

library(dplyr)
library(XML)
library(e1071)  # scaling을 위해 sigmoid() 사용
api_key = "인증키"
options(scipen = 99)


# 1. (ISBN아동_2019년_9-11월_아리랑어린이도서관) 인기 아동도서 상위 10개 수집
## '도서관/지역별 인기대출 도서 조회' API → (도서명, ISBN, 주제분류) 추출
lib_code = 111468  # 아리랑어린이도서관 코드
start_d = '2019-09-01'
end_d = '2019-11-30'
fileUrl = paste0('http://data4library.kr/api/loanItemSrchByLib?authKey=', api_key,
                 '&libCode=', lib_code, '&startDt=', start_d, 
                 '&endDt=', end_d, '&addCode=7&pageSize=10')
xml_doc = xmlTreeParse(fileUrl,useInternal=TRUE)

pop_books = trimws(xpathSApply( xmlRoot(xml_doc), "//bookname", xmlValue)); pop_books
pop_ISBN = trimws(xpathSApply( xmlRoot(xml_doc), "//isbn13", xmlValue)); pop_ISBN
pop_classNum = as.integer(trimws(xpathSApply( xmlRoot(xml_doc), "//class_no", xmlValue))); pop_classNum

any(is.na(c(pop_books, pop_ISBN, pop_addNum, pop_classNum)))  # 정상이면 False


# 2. (2019년_11월_아리랑어린이도서관) 장서/대출 csv 데이터 로드 & 전처리
## columns : 도서명, ISBN, 주제분류, 도서권수, 대출건수
## (도서권수!=0 & 부가기호==7 & 누적대출건수==0) 충족하는 도서 추출 (비인기 아동도서들)
lib_df = read.csv('아리랑어린이도서관 장서 대출목록 (2019년 11월).csv',
                  stringsAsFactors=F )[c(2, 6, 8, 10, 11, 12)]
lib_df$주제분류번호 = as.integer(lib_df$주제분류번호)
colnames(lib_df)
nrow(lib_df)

lib_df = lib_df %>% 
  filter(도서권수 != 0 & !is.na(부가기호) & 부가기호==7) %>% 
  select(-c(부가기호, 도서권수))
lib_df = lib_df %>% 
  filter(대출건수==0) %>% 
  select(-대출건수)
nrow(lib_df)

str(lib_df)
head(lib_df,2)
table(is.na(lib_df))




# - 10개 인기도서 별로, 3~8 진행
## (한 권만 테스트하는 방법 : idx에 숫자 하나 할당하고 for문 안의 명령어 한 줄씩 실행)
recommend_list = list()
for (idx in 1:length(pop_books)) {
  # 3. 인기도서와 '주제분류번호' 같은 비인기도서들 모두(m권) 추출
  ## 길이 m+1인 compare_books, compare_ISBN 벡터에 각각 저장
  ## (1+m < 20)인 경우, 전체 비인기도서들 중 무작위로 추가
  tmp_popbook = pop_books[idx]
  tmp_popISBN = pop_ISBN[idx]
  
  compare_books = c(tmp_popbook,
                    lib_df[ lib_df$주제분류번호 == pop_classNum[idx], ]$도서명)
  compare_ISBN = c(tmp_popISBN,
                   lib_df[ lib_df$주제분류번호 == pop_classNum[idx], ]$ISBN)
  
  ## 수집된 목록이 20권 미만인 경우, 무작위로 부족한 만큼 추출
  if (length(compare_books) < 20) {
    n_sample = 20 - length(compare_books)
    compare_books = c( compare_books,
                       sample(lib_df$도서명[ lib_df$도서명 != compare_books ], 
                              n_sample) )
    compare_ISBN = c( compare_ISBN,
                      sample(lib_df$ISBN[ lib_df$ISBN != compare_ISBN ], 
                             n_sample) )
  }
  cat(tmp_popbook, '도서의 compare_books', length(compare_books), '권 수집 완료\n')
  cat('수집된 벡터들의 길이 :', 
      ifelse( length(compare_books) == length(compare_ISBN) , '정상',
              '문제 발견!!! 확인이 필요합니다'), '\n')
  
  
  # 4. 추출한 (20 ~ m+1)개 각 도서의 키워드, 가중치(표준화) 수집 - 이중 리스트 사용
  ## '도서 키워드 목록' API → 도서명, ISBN, 키워드벡터, 가중치(표준화)벡터 저장
  ## API response 성공적인 도서 10개 수집했을 때 중단 → 최대 11권의 키워드, 가중치(표준화) 수집
  res_list = list()
  list_idx = 1
  for (call_idx in 1:length(compare_books)) {
    tmp_ISBN = compare_ISBN[call_idx]
    tmp_bookname = compare_books[call_idx]
    
    tmp_url = paste0('http://data4library.kr/api/keywordList?authKey=', api_key,'&isbn13=', tmp_ISBN)
    xml_doc = xmlTreeParse(tmp_url, useInternal=TRUE)
    Sys.sleep(1)
    
    tmp_kewords = trimws(xpathSApply( xmlRoot(xml_doc), "//word", xmlValue))
    tmp_weights = as.integer(trimws(xpathSApply( xmlRoot(xml_doc), 
                                                 "//weight", xmlValue)))
    tmp_weights = as.vector(sigmoid(tmp_weights))  # scaling
    
    if (length(tmp_kewords)==0) {
      cat(tmp_bookname, '- 본 도서는 키워드가 제공되지 않아 수집하지 않습니다.\n')
      next()
    }
    
    tmp_res_list = list(bookname = tmp_bookname, ISBN = tmp_ISBN,
                        keywords = tmp_kewords, weights = tmp_weights)
    
    res_list[[list_idx]] = tmp_res_list
    list_idx = list_idx+1
    
    # check
    cat(tmp_bookname, '-',
        length(tmp_res_list$keywords), '개 키워드 수집 완료,',
        ifelse( !any(is.na(tmp_res_list)) , 
                'NA값 없음', 
                'NA값 발견됨' ), '\n')
    
    if (call_idx%%50==0) {
      print('======= 50번째 도서 완료 =======')
    }
    # check
    
    if (length(res_list) > 10) {
      cat('성공적으로', length(res_list), '개 도서를 수집했으므로 중단합니다.\n')
      break()
    }
  }
  # View(res_list)
  
  
  # 5. 수집된 11개 도서에 대한 BOW(Bag of words) 행렬 생성 (영행렬)
  ## 행이름 = c(도서명들) ; 열이름 = c(uniq_kwds)
  booknames = c()
  uniq_kwds = c()
  ISBNs = c()
  
  for (el in res_list) {
    uniq_kwds = unique(c(uniq_kwds, el$keywords))
    booknames = c(booknames, el$bookname)
    ISBNs = c(ISBNs, el$ISBN)
  }
  
  tmp_mat = matrix(0,
                   nrow = length(booknames), 
                   ncol = length(uniq_kwds),
                   dimnames = list(booknames,
                                   uniq_kwds))
  # View(t(tmp_mat))  # 행에 비해 열이 많아서 전치행렬로 확인
  
  
  # 6. 생성된 BOW 행렬에 가중치값 매핑
  ## matrix[ 도서명 , 키워드벡터 ] = 가중치벡터
  for (el in res_list) {
    tmp_mat[el$bookname, el$keywords] = el$weights
  }
  
  
  # 7. 행렬에 내적값 계산(첫 행 · 각 행)해서 열 추가
  ## 도서별 내적값 == 첫 번째 도서(인기도서)와의 연관도
  inner_products = as.vector(tmp_mat[1,] %*% t(tmp_mat))
  tmp_mat = cbind( tmp_mat, inner_products )
  
  
  # 8. inner_products 열 기준으로 행렬 재정렬 → 행렬 rownames 저장
  ## == 모든 도서를 인기도서와의 관련도 순으로 정렬 → 추천도서 목록 저장
  tmp_mat = tmp_mat[ order(tmp_mat[, 'inner_products'], decreasing = T), ]
  tmp_recommend = rownames(tmp_mat)
  
  
  # 9. 재정렬된 추천도서들의 ISBN 저장
  new_idxs = c()
  for (book in tmp_recommend) {
    tmp_idx = grep(book, booknames)
    new_idxs = c(new_idxs, tmp_idx)
  }
  tmp_recommend_ISBN = ISBNs[new_idxs]
  
  # tmp_recommend
  # tmp_recommend_ISBN
  
  recommend_list[[idx]] = list(popbook = tmp_popbook,
                               popISBN = tmp_popISBN,
                               recommend_books = tmp_recommend[-1],
                               recommend_ISBN = tmp_recommend_ISBN[-1])
  
  # check
  print('=====================')
  cat(idx, '번째 도서 :', tmp_popbook, '- 추천목록 생성 완료,',
      ifelse( !any(is.na(recommend_list[[idx]])) , 
              'NA값 없음', 'NA값 발견됨' ), '\n')
  cat('추천도서들의 유사도 :', sort(inner_products[-1], decreasing = T))
  print('=====================')
  # check
}
View(recommend_list)

# # + 결과물 임시 저장
# saveRDS(res_list, '191227_마지막 11권 키워드+가중치 리스트.rds')
# saveRDS(recommend_list, '191227_최종결과물.rds')
