data=read.csv("data3123.csv")
testdata=read.csv("data3220.csv")
Y=data[,2]
X=data[,-(1:2)]

set.seed(1)

library(randomForest)
m=randomForest(x=X,y=Y,ntree=150,important=TRUE)

plot(m)
pred=predict(m,newdata=testdata[,-(1:2)])

save.image("Workspace.RData")

#use weighted MSE as evaluation of prediction
#sum((pred-testdata[,2])^2*testdata[,3])/sum(testdata[,3])
#plot(pred,testdata[,2])
#abline(0,1)

library(WeightedROC)
roc=WeightedROC(pred,testdata[,2],weight=testdata[,3])
plot(TPR~FPR,roc,type="l")
auc=WeightedAUC(roc)
print(auc)
