library(ggplot2)
df<-data.frame(hrs=c(2,3,4,5,6,7,8),
               res=c(0,0,0,0,1,1,1))

ggplot(df,aes(x=hrs,y=res))+ geom_jitter(height=.05, alpha=.1)

model<-glm(res~hrs, data=df, family= "binomial")

summary(model)

ggplot(df,aes(x=hrs,y=res))+ geom_jitter(height=.05, alpha=.1)+
  geom_smooth(method="glm", method.args=list(family="binomial"),se= FALSE)
