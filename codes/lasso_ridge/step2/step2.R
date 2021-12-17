#Imputation_method=2;Feature_selection=3
#1423£»2420
data=read.csv("data1423.csv")
data0=read.csv("data2420.csv")
y=as.matrix(data$Y)
weight=as.matrix(data$Weight)
temp1=data[,-c(1,2,3)]
x=as.matrix(temp1)

set.seed(1)
N=nrow(data)


library(glmnet)
#alpha=1 is the lasso penalty, and alpha=0 the ridge penalty

m=glmnet(x=x,y=y,weights=weight,alpha=1,family="gaussian")
m_cv=cv.glmnet(x=data.matrix(x), y=y, weights=weight, alpha=1,family="gaussian",type.measure='mse',trace.it=TRUE)
#m=glmnet(x=x,y=y,weights=weight,alpha=0,family="gaussian")
#m_cv=cv.glmnet(x=data.matrix(x), y=y, weights=weight, alpha=0,family="gaussian",type.measure='mse',trace.it=TRUE)

plot(m_cv)
print(m_cv)
#coef(m_cv,s="lambda.1se")
pred=predict(m_cv,data.matrix(data0[,-c(1:3)]))
sum((pred-data0[,2])^2*data0[,3])/sum(data0[,3])
plot(pred,data0[,2])
abline(0,1)
#hist(data0[,2]-pred)
#mean(data0[,2]-pred)
library(weights)
wtd.t.test(pred,data0[,2],weighty=data0[,3],alternative="greater")
