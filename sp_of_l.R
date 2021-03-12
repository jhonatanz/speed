library(datasets)
library(dplyr)
library(ggplot2)
df1<-tibble(Expt=0, Run=0, Speed=0)
df<-morley
hist(df$Speed)
boxplot(df$Speed)
df$Expt<-as.factor(df$Expt)
df1<- df %>%
  group_by(Expt)%>%
  summarise(mean = mean(Speed))
df1
plot(x=df$Speed)
ggplot(data = df, aes(x=as.numeric(rownames(df)), y=Speed))+
  geom_point(aes(color=Expt))+
  geom_abline(slope=0, intercept = mean(df$Speed), color ="red")+
  geom_abline(slope=0, intercept = quantile(df$Speed, 0.05), color ="blue")+
  geom_abline(slope=0, intercept = quantile(df$Speed, 0.95), color ="blue")

ggplot(data = df, aes(x=Run, y=Speed))+
  geom_boxplot(aes(color = Expt))


ggplot(data = df, aes(x=Speed))+
  geom_density(aes(color = Expt))+
  geom_density(color="black")

df2<-filter(df, Expt!=1)
ggplot(data = df2, aes(x=Speed))+
  geom_density(aes(color = Expt))+
  geom_density(color="black")
