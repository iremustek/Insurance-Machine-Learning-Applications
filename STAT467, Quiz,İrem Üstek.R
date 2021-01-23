## İrem Üstek, STAT467 Quiz, 12/12/2020

library(ICSNP)
z <- c(-2, -1, 0, 1, 2)
y_1 <- c(5,3, 4,2, 1)
y_2 <- c(-3,-1, -1, 2, 3)
df <- data.frame(y_1,y_2,z)
df
model <- lm(cbind(y_1,y_2)~z,data=df)
summary(model)