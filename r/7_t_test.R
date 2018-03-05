# 1. 분석 목표	
# 집의점유형태, 태어난년도 (1900~2013)

# 2. 가설
# 자가 / 전세,보증부월세,월세 
# 집의점유형태가 자가 인 그룹의 태생년도수가 1970이하가 더 많을것이다.

# 3. 변수 및 분석방법	

# 집의점유형태: 자가, 전세, 보증부월세, 월세 선택
# 년도수: 본인의 태생년도 기입

# 분석방법: 티테스트
# - T 테스트 (두집단의 평균 비교) : 독립변수는 명목, 종속변수는 연속값
# 표준편차가 커질수록 평균에서 어느정도 떨어져있는지 알려주는데
# - T값은 집단간 차이가 커질수록 커지는데, T값에 대한 확률치를 P값으로..

# 4. 측정도구	
# 집의점유형태: 자가, 전세, 보증부월세, 월세 선택
# 년도수: 본인의 태생년도 기입

# 변수명	변수설명	문항내용	Range	SYSMIS, 모름/무응답	변수 종류
# h0906_3	집의 점유형태	1.자가   2.전세   3.보증부월세   4.월세(사글세)  5.기타	N(1~5)	모름/무응답=9	명목
# h0901_5	태어난 연도	년	N(1900~2013)	모름/무응답=9999	연속

raw.03 <- read.spss("data_spss_Koweps2014.sav", to.data.frame = T)
df.d <- raw.03
str(df.d)
summary(df.d)

df.d$homestatus <- df.d$h0906_3
df.d$birthyear <- df.d$h0901_5

table(df.d$homestatus)
df.d$homestatus <- ifelse(df.d$homestatus == 1, "Y", "N")

relHomeBirth <- df.d %>%
        group_by(homestatus) %>%
        summarise(N = n(),
                  M = round(mean(birthyear),2),
                  SD = round(sd(birthyear),2)
        )
relHomeBirth


write.csv(relHomeBirth, "relHomeBirth.csv")

#t-test
#종속변수 ~ 독립변수
t.test(data = df.d,
       birthyear~homestatus,
       var.equal = T)

# Two Sample t-test
# 
# data:  birthyear by homestatus
# t = 10.156, df = 7046, p-value < 2.2e-16

# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#         3.083354 4.558306

# sample estimates:
#         mean in group N mean in group Y 
# 1955.160        1951.339 


ggplot(data = df.d,
       aes(x = homestatus, y = birthyear)) + geom_bar(stat="identity")

