Electronics <- read.csv("/Users/KingsShubo/Desktop/R/train.csv")
###Data Wrangling###
Electronics$category[Electronics$category=="Cameras"] <- NA
Electronics$color[Electronics$color==""]<- NA
Electronics$freeShipping[Electronics$freeShipping==""] <- NA
Electronics$inStock[Electronics$inStock==""]<-NA
Electronics$avRating[Electronics$avRating]<- NA
Electronics$reviewCount[Electronics$reviewCount==""]<- NA
Electronics$listPrice[Electronics$listPrice==""]<-NA
Electronics$shippingPeriod[Electronics$shippingPeriod==""] <- NA
Electronics$PriceUp[Electronics$PriceUp==""]<- NA
Eleinuse <- na.omit(Electronics)
Eleinuse$inStock[Eleinuse$inStock=="2"]<- "0"
Eleinuse$inStock<-as.factor(Eleinuse$inStock)
Eleinuse$freeShipping[Eleinuse$freeShipping=="2"]<- "0"
Eleinuse$freeShipping<-as.factor(Eleinuse$freeShipping)

##Creating trainingset and testset###
set.seed(345)
index_train <- sample(1:nrow(Eleinuse),2/3*nrow(Eleinuse))
trainingset <- Eleinuse[index_train,]
testset <- Eleinuse[-index_train,]

###install packages for getting marginal effects####
installed.packages("mfx")
library(mfx)

####Logistic Regression & marginal effects####
my_result_1 <- glm(PriceUp ~ avRating, family="binomial", data=trainingset)
coef(summary(my_result_1))
logitmfx(formula=PriceUp ~ avRating,data=trainingset)
my_result_2 <- glm(PriceUp ~ avRating+freeShipping, family="binomial", data=trainingset)
coef(summary(my_result_2))
logitmfx(formula=PriceUp ~ avRating+freeShipping,data=trainingset)
my_result_3 <- glm(PriceUp ~ avRating+freeShipping+siteName, family="binomial", data=trainingset)
coef(summary(my_result_3))
logitmfx(formula=PriceUp ~ avRating+freeShipping+siteName,data=trainingset)
my_result_4 <- glm(PriceUp ~ freeShipping+avRating+siteName+brand+color+inStock, 
                   family="binomial", data=trainingset)
coef(summary(my_result_4))


###Predictions###
predictions_1 <- predict(my_result_1,newdata=testset,type="response")
predictions_2 <- predict(my_result_2,newdata=testset,type="response")
predictions_3 <- predict(my_result_3,newdata=testset,type="response")
predictions_4 <- predict(my_result_4,newdata=testset,type="response")

###cut-offs and accuracy rate####
cutoffs<- quantile(predictions_4,seq(0,1,by=0.01))
M <- length(cutoffs)
pred <- matrix(data=NA, nrow=M, ncol=length(predictions_4))
m=1
for (m in 1:M){pred[m,] <- ifelse(predictions_4>cutoffs[m],1,0)
m=m+1}
my_table <- vector("list",101) 
for (m in 1:M) { my_table[[m]] <- cbind(rep(NA,2), NA)}
for (m in 1:M){my_table[[m]] <- table(testset$PriceUp,pred[m,]) }
accuracy <- length(M)
for (m in 1:M) {accuracy[m] <- sum(diag(my_table[[m]])/nrow(testset))}
summary(predictions_4)
plot(cutoffs,accuracy,xlab= "Value of Cut-offs", 
     ylab="Accuracy Rate",lwd=2, main="Figure 1: Cut-offs V.S. Accuracy Rate")
cutoff1 <- quantile(predictions_1,0.87)
pred_1 <- ifelse(predictions_1 > cutoff1,1,0)
Table_1 <- table(testset$PriceUp,pred_1)
accuracy_1 <- sum(diag(Table_1)/nrow(testset))


cutoff2 <- quantile(predictions_2,0.87)
pred_2 <- ifelse(predictions_2 > cutoff2,1,0)
Table_2 <- table(testset$PriceUp,pred_2)
accuracy_2 <- sum(diag(Table_2)/nrow(testset))

cutoff3 <- quantile(predictions_3,0.87)
pred_3 <- ifelse(predictions_3 > cutoff3,1,0)
Table_3 <- table(testset$PriceUp,pred_3)
accuracy_3 <- sum(diag(Table_3)/nrow(testset))

###install packages for ROC###
installed.packages("pROC")
library(pROC)

###ROC & AUCs###
ROC_pred_4 <- roc(testset$PriceUp,pred[88,])
ROC_pred_1 <- roc(testset$PriceUp,pred_1)
ROC_pred_2 <- roc(testset$PriceUp,pred_2)
ROC_pred_3 <- roc(testset$PriceUp,pred_3)
plot(ROC_pred_4,main="Figure 2: ROC Curves")
lines(ROC_pred_1,col="Red")
lines(ROC_pred_2,col="Blue")
lines(ROC_pred_3,col="Green")

AUC_1 <- auc(ROC_pred_1)
AUC_1
AUC_2 <- auc(ROC_pred_2)
AUC_2
AUC_3 <- auc(ROC_pred_3)
AUC_3