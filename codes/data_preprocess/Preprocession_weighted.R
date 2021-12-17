#basic parameters
#Data_choice: whether including 2019 data(before 2019 there is no vaccination data needed, so not using these)
#1:use only 2019 data (more variables with high completeness)
#2:use only 2020 data
#3:use both 2019 and 2020 data (larger sample size)
Data_choice=3
#Y_choose_which:
#1:only SHTFLU12M_A(flu vaccine, 12 months) 
#2:only SHTPNUEV_A(pneumonia vaccine)
#3:both
#4:SHTFLU12M_A,SHTPNUEV_A,SHTSHINGLE_A(shingles for 50+ age)
Y_choose_which=4
#Imputation_method:
#1:Imputing mean
#2:Imputing draw (as there are some categorical data)
#3:Imputation using random forest   (extremely slow, unweighted, but quite recommended in literature)
#4:MICE(Multivariate Imputation via Chained Equations) (preassuming MAR) (slow, unweighted but recommended)
#E-M Imputation cannot be used(preassuming MAR and multivariate normal)
#iterative knn algorithm too slow
#Literature said there is no available efficient imputation algorithm taken weight into consideration, and that there's no evidence against imputing without weight 
Imputation_method=4
#Feature_selection: Algorithm used in third feature selection/dimension reduction
#0 Read in existed selection output
#1 Using SVD and select variables contributing most to eigenvectors with high eigenvalues
#2 Use random forest predictor on imputed data (a bit slow)(no weighted version of random forest, just include weight as a variable and do selection)
#3 Lasso (not using threshold, gives own feature selection)
Feature_selection=1
threshold=50  #number of features selected/number of columns in the final X
set.seed(1234)
library(stats)

#reading data
data1=read.csv("adult19.csv")
data2=read.csv("adult20.csv")
dict=colnames(data1)[which(colnames(data1) %in% colnames(data2))]
data1=data1[,dict]
data2=data2[,dict]

if (Data_choice==1) data=data1
if (Data_choice==2) data=data2
if (Data_choice==3) data=rbind(data1,data2)
rm(data1)
rm(data2)

#splitting Y and X
Y_name=c("SHTFLU12M_A","SHTFLUM_A","SHTFLUY_A","FLUPREG_A","FLUPREG2_A","SHTPNUEV_A",
         "SHTPNEUNB_A","SHTSHINGLE_A","ZOSTAVAX_A","ZOSTAVXYRP_A","ZOSTAWHEN_A","SHINGRIX_A",
         "SHINGRIXNB_A","SHINGRXYRP_A","SHINGWHEN_A","TDAPPREG_A")
Y_where=rep(0,length(Y_name))
for (i in 1:length(Y_name)) Y_where[i]=which(colnames(data)==Y_name[i])
Y_full=data[,Y_where]
X_full=data[,-Y_where]
rm(data)
Weight=X_full[,"WTFA_A"]
X_full=X_full[,-which(colnames(X_full)=="WTFA_A")]

#generating Y, a value in [0,1] representing the willingness to get vaccinated
if (Y_choose_which==1) Y=as.numeric(Y_full[,1]==1)
if (Y_choose_which==2) Y=as.numeric(Y_full[,6]==1)
if (Y_choose_which==3) Y=(as.numeric(Y_full[,1]==1)+as.numeric(Y_full[,6]==1))/2
if (Y_choose_which==4){
  Y=as.numeric(Y_full[,1]==1)+as.numeric(Y_full[,6]==1)
  Y_count=rep(2,length(Y))
  for (i in 1:nrow(Y_full)){
    if ((!is.na(Y_full[i,8]))&&(X_full[i,"AGEP_A"]>=50)&&(X_full[i,"AGEP_A"]<=85)){
      Y[i]=Y[i]+as.numeric(Y_full[i,8]==1)
      Y_count[i]=Y_count[i]+1
    }
  }
  Y=Y/Y_count
}
rm(Y_full)



#X_full=X_full[,-which(colnames(X_full)=="HHX")]
#first feature selection by rate of missingness, not considering gender
#col_completeness=1-colSums(matrix(as.numeric(is.na(X_full)),nrow=nrow(X_full)))/nrow(X_full)
#hist(col_completeness)
#col_remaining=which(col_completeness>0.5)
#X_full=X_full[,col_remaining]

#row_completeness=1-rowSums(matrix(as.numeric(is.na(X_full)),nrow=nrow(X_full)))/ncol(X_full)
#hist(row_completeness)

#second feature selection by ruling out extremely correlated variables
#col_correlated=c()
#for (i in 1:(ncol(X_full)-1)){
#  for (j in (i+1):ncol(X_full)){
#    corr=cor(X_full[,i],X_full[,j],use="pairwise.complete.obs")
#    if ((!is.na(corr))&&(corr>0.9)) col_correlated=c(col_correlated,j)
#  }}
#X_full=X_full[,-col_correlated]
#write.csv(selection,file="first_selection.csv",row.names=FALSE)

#The upper two selection process are similar for all parameters, so we pre-run it and save to reduce time
selected_variables=read.csv("first_selection.csv")
selected_variables=unlist(selected_variables)
names(selected_variables)=c()
X_full=X_full[,selected_variables]



#imputation
if (Imputation_method==1){
  for (j in 1:ncol(X_full)){
    colmean=weighted.mean(X_full[,j],w=Weight,na.rm=TRUE)
    X_full[which(is.na(X_full[,j])),j]=colmean
  }
}

if (Imputation_method==2){
  for (j in 1:ncol(X_full)){
    samples=X_full[which(!is.na(X_full[,j])),j]
    weights=Weight[which(!is.na(X_full[,j]))]
    X_full[which(is.na(X_full[,j])),j]=sample(samples,size=sum(is.na(X_full[,j])),prob=weights)
  }
  rm(weights)
}

if (Imputation_method==3){
  library(missForest)
  X_full=missForest(X_full)
}

if (Imputation_method==4){
  library(mice)
  X_imputed=mice(X_full,m=5,seed=500)
  X_full=complete(X_imputed,1)
}



#third feature selection
if (Feature_selection==0){
  selected_variables=read.csv("feature.csv")
  selected_variables=unlist(selected_variables)
  names(selected_variables)=c()
  X=X_full[,selected_variables]
}

if (Feature_selection==1){
  cut=0.2*ncol(X_full)       #the 'cut' eigenvectors with smallest eigenvalues are considered noise
  X_full_weighted=X_full*sqrt(Weight)
  
  X_svd=svd(X_full_weighted)
  X_U=X_svd$u
  X_d=X_svd$d
  X_V=X_svd$v
  order_d=order(X_d,decreasing=TRUE)
  X_d=X_d[order_d]
  plot(X_d[1:threshold])
  X_U=X_U[,order_d]
  X_V=X_V[,order_d]
  weight=vector(mode="numeric",length=0)
  for (i in 1:ncol(X_full)){
    weight[i]=sum(abs(X_V[i,1:(ncol(X_full)-threshold)]))/sum(abs(X_V[i,(ncol(X_full)-threshold):ncol(X_full)]))
  }
  selected_variables=which(rank(weight)>ncol(X_full)-threshold)
  X=X_full[,selected_variables]
}

if (Feature_selection==2){
  library(randomForest)
  library(varSelRF)
  library(pROC)
  X_full_weight=cbind(Weight,X_full)#X_full including weight as a variable
  rf_ntree=randomForest(x=X_full_weight,y=Y,ntree=150,important=TRUE)
  plot(rf_ntree)
  varImpPlot(rf_ntree)
  importance=rf_ntree$importance
  selected_variables=which(rank(importance)>ncol(X_full)-threshold-1)
  if (selected_variables[1]==1){
    selected_variables=selected_variables[-1]-1
  }else{
    selected_variables=which(rank(importance)>ncol(X_full)-threshold)-1
  }
  X=X_full[,selected_variables]
}
  
if (Feature_selection==3){
  library(glmnet)
  m_lasso=glmnet(x=X_full,y=Y,weights=Weight,alpha=1,family="gaussian")
  plot(m_lasso,xvar="lambda",label=TRUE)
  #trace.it used to visualize the process percentage  
  m_lasso_cv=cv.glmnet(x=data.matrix(X_full),y=Y,weights=Weight,type.measure="mse",alpha=1,family="gaussian",trace.it=TRUE)
  plot(m_lasso_cv)
  print(m_lasso_cv)
  selected_variables=which(coef(m_lasso_cv,s="lambda.1se")!=0)
  if (selected_variables[1]==1){
    selected_variables=selected_variables[-1]
  }
  selected_variables=selected_variables-1
  X=X_full[,selected_variables]
}

data_output=cbind(Y,Weight,X)
write.csv(data_output,file="data.csv")
write.csv(colnames(X_full)[selected_variables],file="feature.csv",row.names=FALSE)
  