# < 각 df에서 중복 도서들의 대출건수를 통합해 하나의 레코드로 만든 df들의 리스트 생성 >
## 0번은 연습이고, 실제 전처리는 1번부터 진행됨

gu = 'seongbook'  # 새로운 리스트 데이터 저장할 때 파일명에 들어갈 구 이름(git을 위해 영어로)

load('//70.12.116.50/교육자료/R자료/6조/2018년 성북구 도서관별 대출목록.RData')


# # 0. sampling - 하나의 도서관에서, 하나의 df에 대해 작업
# tmp_list = sb_lib_list$달빛마루도서관
# tmp_df1 = tmp_list[[1]]
# length(tmp_df1$도서명); length(unique(tmp_df1$도서명))
# 
# uniq_books = unique(tmp_df1$도서명)
# uniq_cnt = c()
# for (idx in 1:length(uniq_books)) {
#   tmp_book = uniq_books[idx]
#   loan_num_vec = tmp_df1$대출건수[tmp_df1$도서명 == tmp_book]
#   uniq_cnt[ idx ] = sum( loan_num_vec )
# }
# 
# res_df1 = data.frame( 도서명 = uniq_books, 대출건수 = uniq_cnt )
# length(res_df1$도서명) == length(unique(res_df1$도서명))  # TRUE이면 중복 없음


# # 0. sampling2 - 하나의 도서관 리스트 전체에서, 1년치 df(6~12개)에 대해 작업
# tmp_list = sb_lib_list$달빛마루도서관
# res_list = list()
# 
# cnt = 0
# for (tmp_df in tmp_list) {
#   cnt = cnt+1
#   cat( length(tmp_df$도서명), '개에서', length(unique(tmp_df$도서명)), '개로 변환\n' )
#   
#   uniq_books = unique(tmp_df$도서명)
#   uniq_cnt = c()
#   for (idx in 1:length(uniq_books)) {
#     tmp_book = uniq_books[idx]
#     loan_num_vec = tmp_df$대출건수[tmp_df$도서명 == tmp_book]
#     uniq_cnt[ idx ] = sum( loan_num_vec )
#   }
#   
#   tmp_res_df = data.frame( 도서명 = uniq_books, 대출건수 = uniq_cnt )
#   cat( length(tmp_res_df$도서명) == length(unique(tmp_res_df$도서명)), '\n' )  # TRUE이면 중복 없음
#   
#   res_list[[ names(tmp_list)[cnt] ]] = tmp_res_df
# }
# str(res_list)



# 1. scale-up - 성북구 전체 리스트에 적용
final_list = list()

for (tmp_lib in names(sb_lib_list)) {
  tmp_lib_list = sb_lib_list[[tmp_lib]]  # 도서관 A의 리스트
  tmp_res_list = list()  # 도서관 A에서 중복을 제거해 새로 만들 리스트
  
  list_idx = 0  # 원래 리스트의 이름목록을 그대로 가져오기 위해, 인덱스 사용
  for (tmp_df in tmp_lib_list) {
    uniq_books = unique(tmp_df$도서명)
    uniq_cnt = c()
    for (idx in 1:length(uniq_books)) {
      tmp_book = uniq_books[idx]
      loan_num_vec = tmp_df$대출건수[tmp_df$도서명 == tmp_book]
      uniq_cnt[idx] = sum( loan_num_vec )
    }
    
    tmp_res_df = data.frame( 도서명 = uniq_books, 대출건수 = uniq_cnt, stringsAsFactors=F )
    
    list_idx = list_idx+1
    tmp_month = names(tmp_lib_list)[list_idx]
    tmp_res_list[[ tmp_month ]] = tmp_res_df
  }
  
  final_list[[tmp_lib]] = tmp_res_list
  cat(tmp_lib, '통합 완료\n')
}

View(final_list)


# 2. RDS 파일로 저장 & 불러와서 확인

file_name = paste('2018', gu, 'library data.rds')
saveRDS(final_list, file = file_name)

final_list_check = readRDS(file_name)
identical(final_list, final_list_check)  # TRUE
View(final_list_check)





# ----------------- 지워 -------------------








list_len = length(tmp_list)
str(tmp_list[[ 12-list_len+2 ]])
str(tmp_list[[ 12 ]])

# 1. 
## 2월~12월 혹은 8월~12월 동안 반복
for (idx in c( (12-list_len+2) : 12)) {
  
}




