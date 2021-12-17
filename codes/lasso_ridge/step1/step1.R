#reading data
data=read.csv("data.csv")


#splitting Y,weight,and X
y=as.matrix(data$Y)
weight=as.matrix(data$Weight)
temp1=data[,-c(1,2,3)]
x=as.matrix(temp1)


#splitting trianing data and test data
set.seed(1)
N=nrow(data)
test_index=sample(N,0.3*N)
train_index=1:N
train_index=train_index[-test_index]
test_data=data[test_index,]
train_data=data[train_index,]


#Model
library(glmnet)
#alpha=1 is the lasso penalty, alpha=0 the ridge penalty

#m=glmnet(x=x[train_index,],y=y[train_index],weights=weight[train_index],alpha=1,family="gaussian")
#m_cv=cv.glmnet(x=data.matrix(x[train_index,]), y=y[train_index], weights=weight[train_index], alpha=1,family="gaussian",type.measure='mse',trace.it=TRUE)
m=glmnet(x=x[train_index,],y=y[train_index],weights=weight[train_index],alpha=0,family="gaussian")
m_cv=cv.glmnet(x=data.matrix(x[train_index,]), y=y[train_index], weights=weight[train_index], alpha=0,family="gaussian",type.measure='mse',trace.it=TRUE)

plot(m_cv)
print(m_cv)
coef(m_cv,s="lambda.1se")
pred=predict(m_cv,data.matrix(x[test_index,]))
sum((pred-y[test_index])^2*weight[test_index])/sum(weight[test_index])




