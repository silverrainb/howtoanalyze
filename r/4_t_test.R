library(foreign)
#성별과 소득의 차이

raw.03 <- read.spss("data_spss_Koweps2014.sav", to.data.frame = T)
df.c <- raw.03

dim(df.c)
str(df.c, list.len = 10)

df.c$sex <- df.c$h0901_4
df.c$income <- df.c$h09_din

class(df.c$sex)
summary(df.c$sex)
table(df.c$sex)
qplot(as.factor(df.c$sex))

table(is.na(df.c$sex))
df.c$sex <- ifelse(df.c$sex == 9, NA, df.c$sex)
table(df.c$sex)

df.c$sex <- ifelse(df.c$sex ==1, "m",
                   ifelse(df.c$sex ==2, "f",
                          df.c$sex))
table(df.c$sex)

#

class(df.c$income)
summary(df.c$income)
qplot(df.c$income) + xlim(0,10000)
table(is.na(df.c$income))

#
# 1. 기술통계표 만들기
# 성별 |사례수 | 평균 | 표준편차
tab.sex <- df.c %>%
        group_by(sex) %>%
        summarise(N = n(),
                  M = round(mean(income),2),
                  SD = round(sd(income),2)
        )
tab.sex
write.csv(tab.sex, "tab_sex.csv")

#

t.test(data = df.c,
        income ~ sex,
        var.equal = T)
# 두집단간의 차이가 통계적으로 유의미함 ( p-value)

ggplot(data = tab.sex, aes(sex,M)) + 
        geom_bar(stat= "identity")
