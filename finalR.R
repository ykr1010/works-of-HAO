datar<-read.csv('data_r.csv',header=T,row.names=1)
str(datar)
table(datar$性別)
table(datar$キャンペーン反応フラグ)

par(family = 'HiraKakuProN-W3')
barplot(table(datar$世代))
hist(datar$キャンペーン前M)
datar$性別<-as.factor(datar$性別)
boxplot(キャンペーン前R~性別,data=datar)

var_result<-var.test(datar$購入金額_201304,datar$購入金額_201404)
var_result
t_test_result<-t.test(datar$購入金額_201304,datar$購入金額_201404)
t_test_result

datar$キャンペーン反応フラグ<-as.factor(datar$キャンペーン反応フラグ)
library(rpart)
tree_result<-rpart(キャンペーン反応フラグ~.,
                   data=datar,
                   method='class',
                   parms=list(split='gini')
                   )
tree_result
summary(tree_result)
tree_result$variable.importance
barplot(tree_result$variable.importance)
library(partykit)
plot(as.party(tree_result),gp=gpar(fontfamily="Hiragino Sans", fontsize=8))

col_selected<-c('キャラクター商品M','キャンペーン前M','飲料M','パンM','おにぎりM','購入金額_201304')
PCAdata<-datar[col_selected]
PCA_result<-prcomp(PCAdata,scale=T)
summary(PCA_result)
barplot(sort(PCA_result$rotation[,1],decreasing=T),las=2,main='PC1',horiz=T)
biplot(PCA_result,choices=c(1,2))
PCA_result$rotation

selected_col<- grep("M$", colnames(datar), value = TRUE)
selected_col<- selected_col[selected_col!= "キャンペーン前M"]
datar_cluster <- datar[selected_col]
str(datar_cluster)
datar_cluster_scaled <- scale(datar_cluster)
set.seed(42)
kmeans_result <- kmeans(datar_cluster_scaled, centers = 3)
kmeans_result
plot(datar_cluster_scaled,col=kmeans_result$cluster)
result_cluster<-kmeans_result$cluster

