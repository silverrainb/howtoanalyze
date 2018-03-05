#회귀분석
raw.02 <- read.csv("02_reg.csv", header =T)
df.b <- raw.02
str(df.b)
summary(df.b)

#lm # linear method
out.reg <- lm(data = df.b, sat~skill)
summary(out.reg) #summary를 이용해야 회귀분석 결과값이 출력됨.
# coefficients:
# Intercept 상수
# Estimate: 회귀계수 B
# p가 0.003 0.05보다 작으니, 유의함
# r스퀘어 : multiple R-squared
ztable(out.reg)
# excel에 ctrl+alt+v로 텍스트로 붙이면 형식이 꺠지지않고 복사가 잘됨!


#간단한그래프
plot(data = df.b, sat~ skill) # 산점도
abline(out.reg) #회귀선 추가


