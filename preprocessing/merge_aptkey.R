apt_info = read.csv("../data/apt_info_dummy.csv")
apt_info %>% head()
apt_info_code = apt_info %>% select(kaptCode,kaptName,doroJuso) 
apt_info_code %>% head()
apt_info_code = apt_info_code %>% mutate(doroJuso = gsub(" ", "", doroJuso))
apt_info_code %>% head() 

### APT list 단지명 도로명 key 합치기
df_total <- read.csv("../data/df_total_cleaning.csv")
df_total %>% head()
apt_list = df_total %>% select(단지명,도로명) %>% 
  unique() %>% mutate(도로명 = paste0("서울특별시강남구",도로명)) %>% 
  mutate(도로명 = gsub(" ", "", 도로명))
apt_list %>% head()
colnames(apt_list) = c("단지명","doroJuso")
##### Merge
nrow(apt_info_code) #171
nrow(apt_list)#692
apt_info_doroJuso = left_join(apt_info_code,apt_list)
apt_info_doroJuso_all = left_join(apt_info_doroJuso, apt_info, by = "kaptCode")
apt_info_doroJuso_all %>% dim() #183 60
apt_info_doroJuso_all %>% colnames()
################################################
df_total %>% dim() # 52881     8
#df_total의 도로명인 apt_list과 apt_info의 doroJuso를 매칭시켜 단지명,도로명에 따른 kaptCode를 만들고자 함
df_total_merge = left_join(df_total, apt_info_doroJuso_all, by="단지명")
df_total_merge %>% dim() #56204    67
df_total %>% select(단지명) %>% unique() %>% nrow()
df_total_merge %>% select(단지명) %>% unique() %>% nrow()
df_total_merge %>% filter(is.na(kaptCode)) %>% select(단지명) %>% unique() %>% nrow()
df_total_merge %>% filter(!is.na(kaptCode)) %>% select(단지명) %>% unique() %>% nrow()
df_total_merge %>% filter(is.na(kaptCode)) %>% dim()
df_total_merge %>% filter(!is.na(kaptCode)) %>% dim()
###kaptCode가 있는 얘들만 분석 (45381 건)

df_total_merge = df_total_merge %>% filter(!is.na(kaptCode))
df_total_merge %>% head()

write.csv(df_total_merge, "../data/preprocessed_df.csv", row.names = FALSE,fileEncoding = 'cp949')
 