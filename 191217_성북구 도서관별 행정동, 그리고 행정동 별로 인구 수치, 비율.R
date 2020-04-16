# 2-2 연령별, 동별 인구 통계

# 1) 성북구 동별 총 인구
population = read.csv('C:/Users/student/R_semi_project/서울시 주민등록인구_(연령별,동별) 통계.csv', header=TRUE)
population = population %>% filter(자치구=='성북구') %>% filter(구분=='계')
population = population %>% select(-기간,-자치구,-구분)
population = population[-1,]
View(population)

# 전체 인구 대비 0~4세 인구 비율 구하기
# population = population %>% 
#   mutate('ratio_BabyToTotal'=
#            round((as.numeric(as.character(population$X0.4세))/
#                     as.numeric(as.character(population$계))*100),2))


# 2) 성북구 동별 영아(0~4세) 인구
# population_baby = population %>% select('행정동','X0.4세','ratio_BabyToTotal')
# 
# View(population_baby)

# 3) 성북구 도서관별 행정동, 그리고 행정동 별로 인구 수치, 비율
sb_adr_plus_pop = inner_join(lib_address,population,by="행정동")
View(sb_adr_plus_pop)
