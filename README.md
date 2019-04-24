# Supervised-Learning
#Number of Types but we learn Linear Regression
library(readxl)
df<-read_excel("bl.xlsx")
View(df)
#Ho-wheather age dosent affect BP
#Ha-Age affects BP
#age is y 
#bp is x
#check normalisation
qqnorm(df$agegrp)
qqline(df$agegrp)
qqnorm(df$bp_before)
qqline(df$bp_before)
hist(df$bp_before)
qqnorm(df$bp_after)
qqline(df$bp_after)

#correlation
pairs(df)
cor(df)

model<-lm(df$agegrp~df$bp_before+df$bp_after)
summary(model)
cal=6.16117+(0.15795*150)+(0.15721*147)
cal
confint(model,level = 0.95)

predict(model,interval="confidence")

predict(model,interval = "prediction")

model_log<-lm(log(df$agegrp)~log(df$bp_before)+log(df$bp_after))
summary(model_log)
