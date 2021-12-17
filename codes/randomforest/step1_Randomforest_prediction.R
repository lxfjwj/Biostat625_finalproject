data=read.csv("data.csv")
Y=data[,2]
X=data[,-(1:2)]

set.seed(1)
N=nrow(data)
test_index=sample(N,0.3*N)
train_index=1:N
train_index=train_index[-test_index]


library(randomForest)
m=randomForest(x=X[train_index,],y=Y[train_index],ntree=150,important=TRUE)

plot(m)
pred=predict(m,newdata=X[test_index,])


save.image("Workspace.RData")

#use weighted MSE as evalutation of prediction
sum((pred-Y[test_index])^2*X[test_index,1])/sum(X[test_index,1])

#library(WeightedROC)
#roc=WeightedROC(pred,Y[test_index],weight=X[test_index,1])
#plot(TPR~FPR,roc,type="l")
#auc=WeightedAUC(roc)
#print(auc)
#save.image("Workspace.RData")
