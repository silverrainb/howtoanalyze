# 1. 분석 목표	
# 태어난년도, 금융기관대출

# 2. 가설
# 태어난년도가 최근일수록 대출액수가 더 높을것이다.

# 3. 변수 및 분석방법	

# 태어난년도기입
# 대출액수기입

# 분석방법: 회귀분석

# 4. 측정도구	
# 태어난년도기입
# 대출액수기입


# 변수명	변수설명	문항내용	Range	SYSMIS, 모름/무응답	변수 종류
# h0901_5	태어난 연도	년	N(1900~2013)	모름/무응답=9999	연속
# h0909_aq1	금융기관대출	연간 부채액(단위: 만원)	N(0~9999998)	"없음=0
# 모름/무응답=9999999"	연속

raw.03 <- read.spss("data_spss_Koweps2014.sav", to.data.frame = T)
df.d <- raw.03
str(df.d)
summary(df.d)

df.d$loan <- df.d$h0909_aq1
df.d$birthyear <- df.d$h0901_5

table(df.d$loan)
table(df.d$birthyear)
summary(df.d$loan)
summary(df.d$birthyear)

table(is.na(df.d$loan))
table(is.na(df.d$birthyear))

class(df.d$loan)
class(df.d$birthyear)


hist(df.d$loan)


dfloannew <- df.d %>% filter(loan > 0)
table(dfloannew$loan)

result2 <- lm(data = dfloannew, loan~birthyear)
summary(result2) 
ztable(result2)

ggplot(dfloannew, aes(birthyear, loan)) + 
        ylim(1,1000) +
        geom_point(size = 3) +
        geom_smooth(method = lm,
                    se = F)

# 이럴때는 그룹으로 나눠서 티테스트를 해보는것도 좋음.
# 몇년전이면 A 그룹, 아니면 B 그룹으로 묶어서.
