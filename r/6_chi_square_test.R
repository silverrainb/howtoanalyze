# 1. 분석 목표	
# 이혼여부, 종교여부

# 2. 가설
# 종교가 없는 그룹이 이혼을 더 많이 할 것이다.

# 3. 변수 및 분석방법	

# 종교선택
# 혼인유무선택

# 분석방법: 카이분석

# 4. 측정도구	
# 종교선택
# 혼인유무선택

# 변수명	변수설명	문항내용	Range	SYSMIS, 모름/무응답	변수 종류
# h0901_12	종교	1.있음                2.없음	N(1~2)	모름/무응답=9	명목
# h0901_11	혼인상태	
# "0.비해당(18세 미만)
# # 1.유배우         
#   2.사별         
#   3.이혼          
#   4.별거          
# # 5.미혼(18세이상, 미혼모 포함)   6.기타(사망 등)"	N(0~6)	모름/무응답=9	명목


raw.03 <- read.spss("data_spss_Koweps2014.sav", to.data.frame = T)
df.c <- raw.03
str(df.c)
summary(df.c)

df.c$religion <- df.c$h0901_12
df.c$status <- df.c$h0901_11

class(df.c$religion)
class(df.c$status)
table(df.c$religion)
table(df.c$status)
df.c$status <- ifelse(df.c$status == 3, 7, df.c$status)
df.c$status <- ifelse(df.c$status == 4, 7, df.c$status)
df.c$status <- ifelse(df.c$status == 5, 7, df.c$status)
table(df.c$status)
df.c$status <- ifelse(df.c$status ==7, "separated", df.c$status)
df.c$status <- ifelse(df.c$status ==1, "married", df.c$status)
table(df.c$status)


newdf.c$religion <- ifelse(df.c$religion == 1, "Y", "N")
table(df.c$religion)


newdf <- df.c %>% filter(df.c$status == "separated" | df.c$status == "married")
table(newdf$status)
table(newdf$religion)

newtab <- table(newdf$religion, newdf$status)
newtab



# 비율을 구할때 무슨값을 기준으로 구할것인지. 
# 열(2)을 기준으로 할지, 행(1)을 기준으로 할지.
#비율표 만들기
proptab <- round(prop.table(newtab,2) * 100, 2)
proptab

#표 두개를 하나로 만들기
tabReligionMarriage <- cbind(newtab,proptab)
tabReligionMarriage
write.csv(tabReligionMarriage,"tab_religion_marriage.csv")

#카이검정하기 ####
chisq.test(newdf$religion, newdf$status, correct=F)

#시각화하기
barplot(newtab, legend = c("Y", "N"), beside = T)

