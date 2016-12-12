Electronics <- read.csv("/Users/KingsShubo/Desktop/R/train.csv")
###Data Wrangling###
Electronics$category[Electronics$category=="Cameras"] <- NA
Electronics$color[Electronics$color==""]<- NA
Electronics$freeShipping[Electronics$freeShipping==""] <- NA
Electronics$inStock[Electronics$inStock==""]<-NA
Electronics$avRating[Electronics$avRating==""]<- NA
Electronics$reviewCount[Electronics$reviewCount==""]<- NA
Electronics$listPrice[Electronics$listPrice==""]<-NA
Electronics$shippingPeriod[Electronics$shippingPeriod==""] <- NA
Electronics$PriceUp[Electronics$PriceUp==""]<- NA
Electronics <- na.omit(Electronics)
Electronics$inStock[Electronics$inStock=="2"]<- "0"
Electronics$inStock<-as.factor(Electronics$inStock)
Electronics$freeShipping[Electronics$freeShipping=="2"]<- "0"
Electronics$freeShipping<-as.factor(Electronics$freeShipping)

data.frame(summary(Electronics$color))
Electronics$color[Electronics$color=="Brown"]<- NA
Electronics$color[Electronics$color=="Dark Tarnish"]<- NA
Electronics$color[Electronics$color=="Stealth Black"]<- NA

plot(Eleinuse$avRating)
Electronics$avRating[Electronics$avRating=="2"] <- NA 
plot(Electronics$reviewCount)
 

summary(Electronics$listPrice)
plot(Electronics$listPrice)
Electronics$listPrice[Electronics$listPrice=="14000"]<-NA

data.frame(summary(Electronics$shippingPeriod))
Electronics$shippingPeriod[Electronics$shippingPeriod=="0"] <- NA
Electronics$shippingPeriod[Electronics$shippingPeriod=="5-7 working days"] <- NA
Electronics$shippingPeriod[Electronics$shippingPeriod=="7-8 Working Days"] <- NA
Electronics <- na.omit(Electronics)
Electronics$avRating_dummies <- table(1:length(Electronics$avRating),as.factor(Electronics$avRating))

length(Electronics$shippingPeriod)
length(Electronics$listPrice)
##Creating trainingset and testset###
set.seed(345)
index_train <- sample(1:nrow(Electronics),2/3*nrow(Electronics))
trainingset <- Electronics[index_train,]
testset <- Electronics[-index_train,]

###install packages for getting marginal effects####
installed.packages("mfx")
library(mfx)

####Logistic Regression & marginal effects####
my_result_1 <- glm(PriceUp ~ avRating_dummies+reviewCount, family="binomial", data=trainingset)
coef(summary(my_result_1))
logitmfx(formula=PriceUp ~ avRating_dummies+reviewCount,data=trainingset)
my_result_2 <- glm(PriceUp ~ avRating_dummies+freeShipping+reviewCount+shippingPeriod, family="binomial", data=trainingset)
coef(summary(my_result_2))
logitmfx(formula=PriceUp ~ avRating_dummies+reviewCount+freeShipping+shippingPeriod,data=trainingset)
my_result_3 <- glm(PriceUp ~ avRating_dummies+reviewCount+freeShipping+shippingPeriod+siteName, family="binomial", data=trainingset)
coef(summary(my_result_3))
logitmfx(formula=PriceUp ~ avRating_dummies+reviewCount+freeShipping+shippingPeriod+siteName,data=trainingset)
my_result_4 <- glm(PriceUp ~ avRating_dummies+reviewCount+freeShipping+shippingPeriod+siteName+inStock+brand+color+listPrice, 
                   family="binomial", data=trainingset)
coef(summary(my_result_4))
logitmfx(formula=PriceUp ~ avRating_dummies+reviewCount+freeShipping+shippingPeriod+siteName+inStock+brand+color+listPrice,data=trainingset)

###Predictions###

predictions_2 <- predict(my_result_2,newdata=testset,type="response")
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
     ylab="Accuracy Rate",lwd=2, main="Figure 3: Cut-offs V.S. Accuracy Rate")


cutoff2 <- quantile(predictions_2,0.91)
pred_2 <- ifelse(predictions_2 > cutoff2,1,0)
Table_2 <- table(testset$PriceUp,pred_2)
accuracy_2 <- sum(diag(Table_2)/nrow(testset))


###install packages for ROC###
installed.packages("pROC")
library(pROC)

###ROC & AUCs###
ROC_pred_4 <- roc(testset$PriceUp,pred[92,])

ROC_pred_2 <- roc(testset$PriceUp,pred_2)

plot(ROC_pred_4,main="Figure 5: ROC Curves")

lines(ROC_pred_2,col="Blue")



AUC_2 <- auc(ROC_pred_2)
AUC_2
