# 1. 분석 목표	가구원수와 주거면적의 관계
# 2. 가설	가구원수가 많을수록 주거면적도 높을것이다.
# 3. 변수 및 분석방법	가구원수: 가구원수가 몇명인지 선택
# 주거면적: 살고있는 집의 평수 기입
# 분석방법: 회귀분석
# 4. 측정도구	가구원수: 가구원수가 몇명인지 선택
# 주거면적: 살고있는 집의 평수 기입

# h0901_1	가구원수	명	N(1~9)	모름/무응답=99
# h0906_5	주거면적	주거면적(평방미터) -소수점 첫째자리에서 반올림 	N(0~9998)	모름/무응답=9999


raw.03 <- read.spss("data_spss_Koweps2014.sav", to.data.frame = T)
df.c <- raw.03
str(df.c)
summary(df.c)

df.c$members <- df.c$h0901_1
df.c$sqm <- df.c$h0906_5

table(is.na(df.c$members))
table(is.na(df.c$sqm))

class(df.c$members)
class(df.c$sqm)
summary(df.c$members)
summary(df.c$sqm)
table(df.c$members)
table(df.c$sqm)


result <- lm(data = df.c, sqm~members)
summary(result) 
ztable(result)


#               Estimate	Std. Error	t value	        Pr(>|t|)
# (Intercept)	53.6092	        0.7821	        68.54	        0.0000
# members	9.0202	        0.2846	        31.70	        0.0000
# Call: lm(formula = sqm ~ members, data = df.c)

# 가구원이 1명 증가할때 9sqm 가 증가

plot(data = df.c, sqm~members) # 산점도
abline(result)
