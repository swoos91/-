# setwd()
rm(list = ls())

library(dplyr)
library(XML)
api_key = "인증키"
options(scipen = 99)  # 지수표기법 대신 숫자 그대로 표시


# 1. ISBN아동_2019년_9-11월_아리랑어린이도서관 인기대출도서 10개 데이터 로드
## API 호출해 응답받은 xml 문서 파싱해 저장(도서명, ISBN, ISBN부가기호, 주제분류)
lib_code = 111468  # 아리랑어린이도서관 코드
start_d = '2019-09-01'
end_d = '2019-11-30'
fileUrl = paste0('http://data4library.kr/api/loanItemSrchByLib?authKey=', api_key,
                 '&libCode=', lib_code, '&startDt=', start_d, 
                 '&endDt=', end_d, '&addCode=7&pageSize=10')
xml_doc = xmlTreeParse(fileUrl,useInternal=TRUE)

pop_books = trimws(xpathSApply( xmlRoot(xml_doc), "//bookname", xmlValue)); pop_books
pop_ISBN = trimws(xpathSApply( xmlRoot(xml_doc), "//isbn13", xmlValue)); pop_ISBN
pop_addNum = trimws(xpathSApply( xmlRoot(xml_doc), "//addition_symbol", xmlValue)); pop_addNum
pop_classNum = trimws(xpathSApply( xmlRoot(xml_doc), "//class_no", xmlValue)); pop_classNum

any(is.na(c(pop_books, pop_ISBN, pop_addNum, pop_classNum)))  # 정상이면 False

# ## 기타 파싱 연습
# rootNode <- xmlRoot(xml_doc)  # 루트노드 따로 저장
# xmlName(rootNode)  # 루트노드의 이름 확인
# names(rootNode)  # 루트노드의 하위노드 확인
# docs_node = rootNode[[3]]  # doc들을 가지고 있는 docs 노드 따로 저장
# doc1_node = docs_node[[1]]  # docs 노드의 첫 번째 doc 노드 따로 저장
# length(names(doc1_node))  # doc 노드의 하위노드 개수 확인
# doc1_node[[11]]  # doc 노드의 11번째 값이 대출건수 값



# 2. 2019년_11월_아리랑어린이도서관 장서/대출 데이터 로드 & 전처리 및 확인
## load columns : 도서명, ISBN, 부가기호, 주제분류번호, 도서권수, 대출건수 
## 도서권수 0인 도서(독본) 제외, 아동도서(부가기호 7)만 추출, 대출건수 0만 추출
lib_df = read.csv('아리랑어린이도서관 장서 대출목록 (2019년 11월).csv',
                  stringsAsFactors=F )[c(2, 6, 8, 10, 11, 12)]
nrow(lib_df)

lib_df = lib_df %>% filter(도서권수 != 0)  # 도서권수 0 인 도서(독본) 제외
lib_df = lib_df %>% 
            filter(!is.na(부가기호) & 부가기호==7) %>% 
            select(-부가기호)  # 아동도서(부가기호 7)만 추출
lib_df = lib_df %>% 
            filter(대출건수==0) %>% 
            select(-대출건수)  # 누적 대출건수 0만 추출
nrow(lib_df)

str(lib_df)
head(lib_df,2)
table(is.na(lib_df))
summary(lib_df)

table(lib_df$주제분류번호)
table(lib_df$주제분류번호)[table(lib_df$주제분류번호) > 20] %>% sort() # 해당하는 도서 많은 주제만 추출
## 813 소설, 990 전기, 375 유아 및 초등 교육, 811 시

table(lib_df$도서권수)
lib_df[ lib_df$도서권수>=5 , ] %>% arrange(도서권수)  # 도서권수 이상치 확인 - 추가 취재 필요?



# 3. 아리랑어린이도서관 장서 전처리 df에서, 10대 인기 도서와 주제분류 겹치는 도서만 추출
matched_books_df = lib_df[ lib_df$주제분류번호 %in% pop_classNum, ]
nrow(matched_books_df)  # 999권

table(pop_classNum)
table(matched_books_df$주제분류번호)

## idea - 겹치는 도서 수가 적정한 911에 대해 핵심 키워드 기반 유사도 분석??
## issue - 겹치는 도서 수가 너무 많은 813.6, 813.8 은 도서 목록을 어떻게 축소??
## issue - 겹치는 도서가 너무 적거나 없는 001, 813.7, 833.6 은 도서 목록을 어떻게 생성??







# 4. sampling - 주제분류 911 도서 대신 추천하기 위해 비교할 도서 목록 생성
## + 전체 df에서 제목 키워드 포함한 도서 추가
pop_books[pop_classNum==911]
pop_ISBN[pop_classNum==911]

## 주제분류 같은(911) 도서 추출
matched_books_df[matched_books_df$주제분류번호==911, ]
## 제목 일부 겹치는 도서 추출
titl_simil_df = lib_df[grepl('why', lib_df$도서명, fixed = TRUE) | 
                         grepl('신화', lib_df$도서명, fixed = TRUE) | 
                         grepl('전설', lib_df$도서명, fixed = TRUE), ]
titl_simil_df

## 유사성 비교할 최종 목록 생성
compare_books = c(pop_books[pop_classNum==911], 
                  matched_books_df[matched_books_df$주제분류번호==911, ]$도서명,
                  titl_simil_df$도서명); compare_books
compare_ISBN = c(pop_ISBN[pop_classNum==911],
                 matched_books_df[matched_books_df$주제분류번호==911, ]$ISBN,
                 titl_simil_df$ISBN); compare_ISBN
length(compare_books) == length(compare_ISBN)  # 정상이면 TRUE



## Error/issue - API 호출결과 키워드 0개인 도서 빈번
# 5. 각 도서별 키워드 및 상세정보 저장한 이중 리스트 생성 (API 사용)
res_list = list()
list_idx = 1
for (idx in 1:length(compare_books)) {
  tmp_ISBN = compare_ISBN[idx]
  tmp_bookname = compare_books[idx]
  
  tmp_url = paste0('http://data4library.kr/api/keywordList?authKey=', api_key,'&isbn13=', tmp_ISBN)
  xml_doc = xmlTreeParse(tmp_url, useInternal=TRUE)
  Sys.sleep(1)
  
  tmp_kewords = trimws(xpathSApply( xmlRoot(xml_doc), "//word", xmlValue))
  tmp_weights = as.numeric(trimws(xpathSApply( xmlRoot(xml_doc), "//weight", xmlValue)))
  
  if (length(tmp_kewords)==0) {
    cat(tmp_bookname, '- 본 도서는 키워드가 제공되지 않아 수집하지 않습니다.\n')
    next()
  }
  
  tmp_res_list = list(bookname = tmp_bookname, ISBN = tmp_ISBN,
                      keywords_df = data.frame(keyword = tmp_kewords, weight = tmp_weights, stringsAsFactors = F))
  
  res_list[[list_idx]] = tmp_res_list
  list_idx = list_idx+1
  
  # check
  cat(tmp_bookname, '-',
      nrow(tmp_res_list$keywords_df), '개 키워드 수집 완료',
      any(is.na(tmp_res_list)), 
      any(is.na(tmp_res_list$keywords_df)), '\n')  # 정상이면 F, F
  
  # if (idx%%50==0) {
  #   print('======= 50번째 완료 =======')
  # }
  # check
  
}
length(res_list)
View(res_list)



# 6. 키워드 가중치를 근거로, 도서쌍의 내적값을 구해, 키워드 유사도 순으로 우선순위 결정
bookname_vec = c()
ISBN_vec = c()
inner_product_vec = c()

for (tmp_list in res_list) {
  bookname_vec = c(bookname_vec, tmp_list$bookname)
  ISBN_vec = c(ISBN_vec, tmp_list$ISBN)
  
  tmp_df = inner_join( res_list[[1]]$keywords_df, 
                       tmp_list$keywords_df,
                       by='keyword') %>% 
            mutate(multiple = weight.x * weight.y)
  inner_product_vec = c(inner_product_vec, sum(tmp_df$multiple))
}

res_df = data.frame(bookname = bookname_vec,
                    ISBN = ISBN_vec,
                    inner_product = inner_product_vec,
                    stringsAsFactors = F) %>% 
          arrange(desc(inner_product))
res_df

