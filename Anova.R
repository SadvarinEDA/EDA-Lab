library(dplyr)
df <- read.csv("df.csv")
summary(df)
group_by(df,color)%>%
  summarise(count=n(),mean=mean(response, na.rm=TRUE))
anova<-aov(response~color,data=df)
summary(anova)

TukeyHSD(anova)