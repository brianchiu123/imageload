library(reshape2)
library(tidyverse)
library(corrplot)
library(caret)

Boston <- read.csv("train.csv")
df<-data.frame(Boston)
df$ID <- NULL
print(df)

summary(df)

#target is medv

#房價計算
houseprice<-ggplot(df, aes(x=medv)) + 
  geom_histogram()
print(houseprice + labs(y="count", x = "price($1000美元)"))
#房價呈現常態分布
housepricedense<-ggplot(df, aes(x=medv)) + 
  geom_histogram(aes(y=..density..), colour="black")+
  geom_density(alpha=.2, fill="#FF6666")
print(housepricedense + labs(y="density", x = "price($1000美元)"))




#calculate feature correlation matrix
CorMat <- cor(df[ ,c("crim","zn","chas","nox","rm","age","dis","rad","tax","ptratio","black","lstat","medv")])



#draw 1. correlation picture
get_upper_tri <- function(CorMat){
  CorMat[upper.tri(CorMat)]<- NA
  return(CorMat)
}

get_lower_tri <- function(CorMat){
  CorMat[lower.tri(CorMat)]<- NA
  return(CorMat)
}

reorder <- function(CorMat){
  dd <- as.dist((1-CorMat)/2)
  hc <- hclust(dd)
  CorMar <- CorMat[hc$order, hc$order]
}


CorMat <- reorder(CorMat)
upper_tri <- get_upper_tri(CorMat)
lower_tri <- get_lower_tri(CorMat)
meltNum <- melt(lower_tri, na.rm = T)
meltColor <- melt(upper_tri, na.rm = T)

ggplot() +
  labs(x = NULL, y = NULL) +
  geom_tile(data = meltColor, 
            mapping = aes(Var2, Var1, 
                          fill = value)) +
  geom_text(data = meltNum,
            mapping = aes(Var2, Var1,
                          label = round(value, digit = 2))) +
  scale_x_discrete(position = "top") +
  scale_fill_gradient(low = "white", high = "firebrick4",
                      limit = c(-1,1), name = "Pearson\nCorrelation") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
#draw 2. correlation picture
ggcorrplot::ggcorrplot(cor(df), type = "lower", lab = TRUE)

#draw 3. correlation picture
ggcorrplot::ggcorrplot(cor(df), lab = TRUE)



#點狀圖
#df[, 14]
#colnames(df)
for(i in c(1:13)){
  fearturescatter<-ggplot(df, aes(x=df[,i], y=df[,14]))+geom_point()
  print(fearturescatter + labs(y="medv", x = colnames(df)[i]))
  
}




#use caret random forest model find feature importance
rfmodel <- train(df[ ,c("crim","zn","chas","nox","rm","age","dis","rad","tax","ptratio","black","lstat")],df[ ,c("medv")], method="rf")
rfImp <- varImp(rfmodel, scale=T)
rfImp
plot(rfImp, main='Variable Importance')






