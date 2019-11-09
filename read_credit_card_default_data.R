# Eric Smith
# August 2019
# Credit Card Default project for MSDS 498, Northwestern University


my.path <- 'C://Users//ems97//OneDrive//Documents//MSPA//498//'
my.file <- paste(my.path,'credit_card_default.RData',sep='')

credit_card_default <- readRDS(my.file)

#Clean up PAY variables
credit_card_default[credit_card_default$PAY_0==-1,7] <- 0
credit_card_default[credit_card_default$PAY_0==-2,7] <- 0
credit_card_default[credit_card_default$PAY_2==-1,8] <- 0
credit_card_default[credit_card_default$PAY_2==-2,8] <- 0
credit_card_default[credit_card_default$PAY_3==-1,9] <- 0
credit_card_default[credit_card_default$PAY_3==-2,9] <- 0
credit_card_default[credit_card_default$PAY_4==-1,10] <- 0
credit_card_default[credit_card_default$PAY_4==-2,10] <- 0
credit_card_default[credit_card_default$PAY_5==-1,11] <- 0
credit_card_default[credit_card_default$PAY_5==-2,11] <- 0
credit_card_default[credit_card_default$PAY_6==-1,12] <- 0
credit_card_default[credit_card_default$PAY_6==-2,12] <- 0

credit_card_default[credit_card_default$EDUCATION==0,4] <- 4
credit_card_default[credit_card_default$EDUCATION==5,4] <- 4
credit_card_default[credit_card_default$EDUCATION==6,4] <- 4

colnames(credit_card_default)[7] <- "PAY_1"

#Define bins for age variable
library(woeBinning)
df <- credit_card_default[,c('DEFAULT','AGE')]
binning <- woe.binning(df,'DEFAULT','AGE')

credit_card_default$Age_18_25 <- 0
credit_card_default[credit_card_default$AGE <= 25,c("Age_18_25")] <- 1

credit_card_default$Age_26_40 <- 0
credit_card_default[credit_card_default$AGE > 25 & credit_card_default$AGE <=40,c("Age_26_40")] <- 1

credit_card_default$Age_41_100 <- 0
credit_card_default[credit_card_default$AGE > 40,c("Age_41_100")] <- 1

#Create additional derived variables

credit_card_default$Avg_Bill_Amt <- (credit_card_default$BILL_AMT1+credit_card_default$BILL_AMT2+credit_card_default$BILL_AMT3+credit_card_default$BILL_AMT4+credit_card_default$BILL_AMT5+credit_card_default$BILL_AMT6)/6

credit_card_default$Avg_Pmt_Amt <- (credit_card_default$PAY_AMT1+credit_card_default$PAY_AMT2+credit_card_default$PAY_AMT3+credit_card_default$PAY_AMT4+credit_card_default$PAY_AMT5+credit_card_default$PAY_AMT6)/6

credit_card_default$Pmt_Ratio2 <- ifelse(credit_card_default$PAY_AMT1==0,ifelse(credit_card_default$BILL_AMT1==0,100,0),credit_card_default$BILL_AMT2/credit_card_default$PAY_AMT1*100)
credit_card_default$Pmt_Ratio3 <- ifelse(credit_card_default$PAY_AMT2==0,ifelse(credit_card_default$BILL_AMT2==0,100,0),credit_card_default$BILL_AMT3/credit_card_default$PAY_AMT2*100)
credit_card_default$Pmt_Ratio4 <- ifelse(credit_card_default$PAY_AMT3==0,ifelse(credit_card_default$BILL_AMT3==0,100,0),credit_card_default$BILL_AMT4/credit_card_default$PAY_AMT3*100)
credit_card_default$Pmt_Ratio5 <- ifelse(credit_card_default$PAY_AMT4==0,ifelse(credit_card_default$BILL_AMT4==0,100,0),credit_card_default$BILL_AMT5/credit_card_default$PAY_AMT4*100)
credit_card_default$Pmt_Ratio6 <- ifelse(credit_card_default$PAY_AMT5==0,ifelse(credit_card_default$BILL_AMT5==0,100,0),credit_card_default$BILL_AMT6/credit_card_default$PAY_AMT5*100)

credit_card_default$Pmt_Ratio2[credit_card_default$Pmt_Ratio2 < 0] <- 100
credit_card_default$Pmt_Ratio3[credit_card_default$Pmt_Ratio3 < 0] <- 100
credit_card_default$Pmt_Ratio4[credit_card_default$Pmt_Ratio4 < 0] <- 100
credit_card_default$Pmt_Ratio5[credit_card_default$Pmt_Ratio5 < 0] <- 100
credit_card_default$Pmt_Ratio6[credit_card_default$Pmt_Ratio6 < 0] <- 100

credit_card_default$Pmt_Ratio2[credit_card_default$Pmt_Ratio2 > 100] <- 100
credit_card_default$Pmt_Ratio3[credit_card_default$Pmt_Ratio3 > 100] <- 100
credit_card_default$Pmt_Ratio4[credit_card_default$Pmt_Ratio4 > 100] <- 100
credit_card_default$Pmt_Ratio5[credit_card_default$Pmt_Ratio5 > 100] <- 100
credit_card_default$Pmt_Ratio6[credit_card_default$Pmt_Ratio6 > 100] <- 100

credit_card_default$Avg_Pmt_Ratio <- (credit_card_default$Pmt_Ratio2+credit_card_default$Pmt_Ratio3+credit_card_default$Pmt_Ratio4+credit_card_default$Pmt_Ratio5+credit_card_default$Pmt_Ratio6)/5

credit_card_default$Util1 <- credit_card_default$BILL_AMT1/credit_card_default$LIMIT_BAL*100
credit_card_default$Util2 <- credit_card_default$BILL_AMT2/credit_card_default$LIMIT_BAL*100
credit_card_default$Util3 <- credit_card_default$BILL_AMT3/credit_card_default$LIMIT_BAL*100
credit_card_default$Util4 <- credit_card_default$BILL_AMT4/credit_card_default$LIMIT_BAL*100
credit_card_default$Util5 <- credit_card_default$BILL_AMT5/credit_card_default$LIMIT_BAL*100
credit_card_default$Util6 <- credit_card_default$BILL_AMT6/credit_card_default$LIMIT_BAL*100

credit_card_default$Avg_Util <- (credit_card_default$Util1+credit_card_default$Util2+credit_card_default$Util3+credit_card_default$Util4+credit_card_default$Util5+credit_card_default$Util6)/5

credit_card_default$Bal_Growth_6mo <- credit_card_default$BILL_AMT6-credit_card_default$BILL_AMT1

credit_card_default$Util_Growth_6mo <- credit_card_default$Util6-credit_card_default$Util1

credit_card_default$Max_Bill_Amt <- 0

for (i in 1:nrow(credit_card_default)) {
	credit_card_default$Max_Bill_Amt[i] <- max(credit_card_default$BILL_AMT1[i],credit_card_default$BILL_AMT2[i],credit_card_default$BILL_AMT3[i],credit_card_default$BILL_AMT4[i],credit_card_default$BILL_AMT5[i],credit_card_default$BILL_AMT6[i])
}

credit_card_default$Max_Pmt_Amt <- 0

for (i in 1:nrow(credit_card_default)) {
	credit_card_default$Max_Pmt_Amt[i] <- max(credit_card_default$PAY_AMT1[i],credit_card_default$PAY_AMT2[i],credit_card_default$PAY_AMT3[i],credit_card_default$PAY_AMT4[i],credit_card_default$PAY_AMT5[i],credit_card_default$PAY_AMT6[i])
}

credit_card_default$Max_DLQ <- 0

for (i in 1:nrow(credit_card_default)) {
	credit_card_default$Max_DLQ[i] <- max(credit_card_default$PAY_1[i],credit_card_default$PAY_2[i],credit_card_default$PAY_3[i],credit_card_default$PAY_4[i],credit_card_default$PAY_5[i],credit_card_default$PAY_6[i])
}

col.list <- c('DEFAULT','LIMIT_BAL','SEX','EDUCATION','MARRIAGE','train','test','validate','Age_18_25','Age_26_40','Avg_Bill_Amt','Avg_Pmt_Amt','Avg_Pmt_Ratio','Avg_Util','Bal_Growth_6mo','Util_Growth_6mo','Max_Bill_Amt','Max_Pmt_Amt','Max_DLQ')

col.list.eda <- c('DEFAULT','LIMIT_BAL','SEX','EDUCATION','MARRIAGE','Age_18_25','Age_26_40','Avg_Bill_Amt','Avg_Pmt_Amt','Avg_Pmt_Ratio','Avg_Util','Bal_Growth_6mo','Util_Growth_6mo','Max_Bill_Amt','Max_Pmt_Amt','Max_DLQ')
cc <- credit_card_default[,col.list]


cc_eda <- credit_card_default[,col.list.eda]

cc.oner <- OneR(DEFAULT ~ ., data=cc_eda, verbose=TRUE)\

#Create Boxplots
par(mfrow=c(2,5))
boxplot(LIMIT_BAL ~ DEFAULT, data=cc, ylim=c(0,600000),main="Figure 4.1")
boxplot(Avg_Util ~ DEFAULT, data=cc, ylim=c(-10,650),main="Figure 4.2")
boxplot(Avg_Pmt_Amt ~ DEFAULT, data=cc, ylim=c(-10,20000),main="Figure 4.3")
boxplot(Avg_Bill_Amt ~ DEFAULT, data=cc, ylim=c(-60000,200000),main="Figure 4.4")
boxplot(Avg_Pmt_Ratio ~ DEFAULT, data=cc, ylim=c(0,100),main="Figure 4.5")
boxplot(Max_Pmt_Amt ~ DEFAULT, data=cc, ylim=c(-10,50000),main="Figure 4.6")
boxplot(Max_Bill_Amt ~ DEFAULT, data=cc, ylim=c(-100,300000),main="Figure 4.7")
boxplot(Bal_Growth_6mo ~ DEFAULT, data=cc, ylim=c(-100000,100000),main="Figure 4.8")
boxplot(Util_Growth_6mo ~ DEFAULT, data=cc, ylim=c(-100,100),main="Figure 4.9")

#Random Forest Model
library(randomForest)

#Modify data types for random forest package requirements
cc_rf <- cc
cc_rf$DEFAULT <- as.factor(cc_rf$DEFAULT)
cc_rf$SEX <- as.factor(cc_rf$SEX)
cc_rf$EDUCATION <- as.factor(cc_rf$EDUCATION)
cc_rf$MARRIAGE <- as.factor(cc_rf$MARRIAGE)
cc_rf$Age_18_25 <- as.factor(cc_rf$Age_18_25)
cc_rf$Age_26_40 <- as.factor(cc_rf$Age_26_40)
cc_rf$Avg_Bill_Amt <- as.numeric(cc_rf$Avg_Bill_Amt)
cc_rf$Avg_Pmt_Amt <- as.numeric(cc_rf$Avg_Pmt_Amt)
cc_rf$Avg_Pmt_Ratio <- as.numeric(cc_rf$Avg_Pmt_Ratio)
cc_rf$Avg_Util <- as.numeric(cc_rf$Avg_Util)
cc_rf$Bal_Growth_6mo <- as.numeric(cc_rf$Bal_Growth_6mo)
cc_rf$Util_Growth_6mo <- as.numeric(cc_rf$Util_Growth_6mo)
cc_rf$Max_Bill_Amt <- as.numeric(cc_rf$Max_Bill_Amt)
cc_rf$Max_DLQ <- as.numeric(cc_rf$Max_DLQ)
train=which(cc_rf$train==1)

#Create random forest model
set.seed(100)
rf.cc=randomForest(DEFAULT ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + Age_18_25 + Age_26_40 + Avg_Bill_Amt + Avg_Pmt_Amt + Avg_Pmt_Ratio + Avg_Util + Bal_Growth_6mo + Util_Growth_6mo + Max_Bill_Amt + Max_Pmt_Amt + Max_DLQ,data=cc_rf[train,],mtry=6,importance=TRUE)

var_excl <- c("DEFAULT","train","test","validate")
rf_train_result <- cbind(rf.cc$predicted,cc_rf[train,-which(names(cc_rf) %in% var_excl)])
colnames(rf_train_result)[1] <- "DEFAULT"

#Validate on test set
ordered_data <- cbind(cc_rf$DEFAULT[train],rf.cc$predicted)
colnames(ordered_data) <- c("label","pred")
ordered_data <- as.data.frame(ordered_data)
ordered_data <- ordered_data[order(ordered_data$label),]
roc(ordered_data$label,ordered_data$pred)

test=which(cc_rf$test==1)
rf.pred <- predict(rf.cc,newdata=cc_rf[test,])

sum(rf.pred==0 & cc_rf[test,"DEFAULT"]==0)
sum(rf.pred==1 & cc_rf[test,"DEFAULT"]==1)
sum(rf.pred==1 & cc_rf[test,"DEFAULT"]==0)
sum(rf.pred==0 & cc_rf[test,"DEFAULT"]==1)

#Create ROC curves for Random Forest model
library(pROC)
roc(ordered_data$label,ordered_data$pred)
test=which(cc_rf$test==1)
rf.pred <- predict(rf.cc,newdata=cc_rf[test,])

ordered_data <- cbind(cc_rf$DEFAULT[test],rf.pred)
colnames(ordered_data) <- c("label","pred")
ordered_data <- as.data.frame(ordered_data)
ordered_data <- ordered_data[order(ordered_data$label),]
roc(ordered_data$label,ordered_data$pred)

#Refine random forest model
set.seed(100)
rf.cc=randomForest(DEFAULT ~ LIMIT_BAL + Avg_Bill_Amt + Avg_Pmt_Amt + Avg_Pmt_Ratio + Avg_Util + Bal_Growth_6mo + Util_Growth_6mo + Max_Bill_Amt + Max_Pmt_Amt + Max_DLQ,data=cc_rf[train,],mtry=6,importance=TRUE)

ordered_data <- cbind(cc_rf$DEFAULT[train],rf.cc$predicted)
colnames(ordered_data) <- c("label","pred")
ordered_data <- as.data.frame(ordered_data)
ordered_data <- ordered_data[order(ordered_data$label),]
roc(ordered_data$label,ordered_data$pred)

test=which(cc_rf$test==1)

set.seed(100)
rf.cc=randomForest(DEFAULT ~ LIMIT_BAL + Avg_Bill_Amt + Avg_Pmt_Amt + Avg_Pmt_Ratio + Avg_Util + Bal_Growth_6mo + Util_Growth_6mo + Max_Bill_Amt + Max_Pmt_Amt + Max_DLQ,data=cc_rf[train,],mtry=5,importance=TRUE)


ordered_data <- cbind(cc_rf$DEFAULT[train],rf.cc$predicted)
colnames(ordered_data) <- c("label","pred")
ordered_data <- as.data.frame(ordered_data)
ordered_data <- ordered_data[order(ordered_data$label),]
roc(ordered_data$label,ordered_data$pred)

rf.pred <- predict(rf.cc,newdata=cc_rf[test,])

ordered_data <- cbind(cc_rf$DEFAULT[test],rf.pred)
colnames(ordered_data) <- c("label","pred")
ordered_data <- as.data.frame(ordered_data)
ordered_data <- ordered_data[order(ordered_data$label),]
roc(ordered_data$label,ordered_data$pred)


sum(rf.pred==0 & cc_rf[test,"DEFAULT"]==0)
sum(rf.pred==1 & cc_rf[test,"DEFAULT"]==1)
sum(rf.pred==1 & cc_rf[test,"DEFAULT"]==0)
sum(rf.pred==0 & cc_rf[test,"DEFAULT"]==1)

#################
#Gradient boosting model

library(gbm)

#Change variable types for package requirements
cc$DEFAULT <- as.factor(as.character(cc$DEFAULT))
cc$SEX <- as.factor(as.character(cc$SEX))
cc$EDUCATION <- as.factor(as.character(cc$EDUCATION))
cc$MARRIAGE <- as.factor(as.character(cc$MARRIAGE))
cc$Age_18_25 <- as.factor(as.character(cc$Age_18_25))
cc$Age_26_40 <- as.factor(as.character(cc$Age_26_40))

#Create model
set.seed(100)
gbm.cc <- gbm(DEFAULT ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + Age_18_25 + Age_26_40 + Avg_Bill_Amt + Avg_Pmt_Amt + Avg_Pmt_Ratio + Avg_Util + Bal_Growth_6mo + Util_Growth_6mo + Max_Bill_Amt + Max_Pmt_Amt + Max_DLQ,data=cc[train,],n.trees=1500)

gbm.train <- predict(gbm.cc,newdata=cc[train,],type="response",n.trees=1500)

gbm.train.resp <- gbm.train
gbm.train.resp[gbm.train >= 0.5] <- 1
gbm.train.resp[gbm.train < 0.5] <- 0

ordered_data <- cbind(cc$DEFAULT[train],gbm.train.resp)
colnames(ordered_data) <- c("label","pred")
ordered_data <- as.data.frame(ordered_data)
ordered_data <- ordered_data[order(ordered_data$label),]
roc(ordered_data$label,ordered_data$pred)

sum(gbm.train.resp==0 & cc[train,"DEFAULT"]==0)
sum(gbm.train.resp==1 & cc[train,"DEFAULT"]==1)
sum(gbm.train.resp==1 & cc[train,"DEFAULT"]==0)
sum(gbm.train.resp==0 & cc[train,"DEFAULT"]==1)

#Evaluate model on test set
gbm.test <- predict(gbm.cc,newdata=cc[test,],type="response",n.trees=1500)

gbm.test.resp <- gbm.test
gbm.test.resp[gbm.test>= 0.5] <- 1
gbm.test.resp[gbm.test< 0.5] <- 0

ordered_data <- cbind(cc$DEFAULT[test],gbm.test.resp)
colnames(ordered_data) <- c("label","pred")
ordered_data <- as.data.frame(ordered_data)
ordered_data <- ordered_data[order(ordered_data$label),]
roc(ordered_data$label,ordered_data$pred)

sum(gbm.test.resp==0 & cc[test,"DEFAULT"]==0)
sum(gbm.test.resp==1 & cc[test,"DEFAULT"]==1)
sum(gbm.test.resp==1 & cc[test,"DEFAULT"]==0)
sum(gbm.test.resp==0 & cc[test,"DEFAULT"]==1)

#Refine gradient boosting model
set.seed(100)
gbm.cc <- gbm(DEFAULT ~ Avg_Bill_Amt + Avg_Pmt_Ratio + Avg_Util + Bal_Growth_6mo + Util_Growth_6mo + Max_Bill_Amt + Max_Pmt_Amt + Max_DLQ,data=cc[train,],n.trees=1500)

gbm.train <- predict(gbm.cc,newdata=cc[train,],type="response",n.trees=1500)

gbm.train.resp <- gbm.train
gbm.train.resp[gbm.train >= 0.5] <- 1
gbm.train.resp[gbm.train < 0.5] <- 0

ordered_data <- cbind(cc$DEFAULT[train],gbm.train.resp)
colnames(ordered_data) <- c("label","pred")
ordered_data <- as.data.frame(ordered_data)
ordered_data <- ordered_data[order(ordered_data$label),]
roc(ordered_data$label,ordered_data$pred)

sum(gbm.train.resp==0 & cc[train,"DEFAULT"]==0)
sum(gbm.train.resp==1 & cc[train,"DEFAULT"]==1)
sum(gbm.train.resp==1 & cc[train,"DEFAULT"]==0)
sum(gbm.train.resp==0 & cc[train,"DEFAULT"]==1)

gbm.test <- predict(gbm.cc,newdata=cc[test,],type="response",n.trees=1500)

gbm.test.resp <- gbm.test
gbm.test.resp[gbm.test>= 0.5] <- 1
gbm.test.resp[gbm.test< 0.5] <- 0

ordered_data <- cbind(cc$DEFAULT[test],gbm.test.resp)
colnames(ordered_data) <- c("label","pred")
ordered_data <- as.data.frame(ordered_data)
ordered_data <- ordered_data[order(ordered_data$label),]
roc(ordered_data$label,ordered_data$pred)

sum(gbm.test.resp==0 & cc[test,"DEFAULT"]==0)
sum(gbm.test.resp==1 & cc[test,"DEFAULT"]==1)
sum(gbm.test.resp==1 & cc[test,"DEFAULT"]==0)
sum(gbm.test.resp==0 & cc[test,"DEFAULT"]==1)

###################
#Logistic regression model

#Modify variables for model requirements
#Log-transform certain skewed variables
cc_glm <- cc
cc_glm$LIMIT_BAL <- log(cc_glm$LIMIT_BAL)
cc_glm$Avg_Pmt_Amt <- log(cc_glm$Avg_Pmt_Amt + 1)
cc_glm$Max_Pmt_Amt <- log(cc_glm$Max_Pmt_Amt + 1)
cc_glm$Avg_Util <- log(cc_glm$Avg_Util + 28)
cc_glm$Max_Bill_Amt <- log(cc_glm$Max_Bill_Amt + 6030)
cc_glm$Max_DLQ[cc_glm$Max_DLQ < 2] <- 0
cc_glm$Max_DLQ[cc_glm$Max_DLQ >= 2] <- 1

#Create model
glm.cc <- glm(DEFAULT ~ poly(LIMIT_BAL,2) + SEX + EDUCATION + MARRIAGE + Age_18_25 + Age_26_40 + Avg_Bill_Amt + poly(Avg_Pmt_Amt,2) + Avg_Pmt_Ratio + Avg_Util + poly(Bal_Growth_6mo,2) + Util_Growth_6mo + Max_Bill_Amt + poly(Max_Pmt_Amt,2) + Max_DLQ,data=cc_glm[train,],family=binomial)

#Stepwise variable selection
library(MASS)
glm.cc.final <- stepAIC(glm.cc)

library(stargazer)
out.path <- 'C://Users//ems97//OneDrive//Documents//MSPA//498//';
file.name <- 'logistic.html';
stargazer(glm.cc.final, type=c('html'),out=paste(out.path,file.name,sep=''),
	align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE)

glm.train.resp <- glm.cc.final$fitted.values

ordered_data <- cbind(cc$DEFAULT[train],glm.train.resp)
colnames(ordered_data) <- c("label","pred")
ordered_data <- as.data.frame(ordered_data)
ordered_data <- ordered_data[order(ordered_data$label),]
glm_roc <- roc(ordered_data$label,ordered_data$pred)
plot(glm_roc)

coords(roc=glm_roc,x=c('best'),
input=c('threshold','specificity','sensitivity'),
ret=c('threshold','specificity','sensitivity'),
as.list=TRUE
)

glm.train.resp[glm.cc.final$fitted.values >= 0.2] <- 1
glm.train.resp[glm.cc.final$fitted.values < 0.2] <- 0

sum(glm.train.resp==0 & cc_glm[train,"DEFAULT"]==0)
sum(glm.train.resp==1 & cc_glm[train,"DEFAULT"]==1)
sum(glm.train.resp==1 & cc_glm[train,"DEFAULT"]==0)
sum(glm.train.resp==0 & cc_glm[train,"DEFAULT"]==1)

my.df <- as.data.frame(cbind(glm.cc.final$fitted.values,cc_glm[train,"DEFAULT"]));
colnames(my.df) <- c("model.score","response")

#Prepare for K-S Statistic table
decile.pts <- quantile(my.df$model.score,
			probs=c(0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95));

my.df$model.decile <- cut(my.df$model.score,breaks=c(0,decile.pts,1),
			labels=rev(c('01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20'))
			);
table(my.df$model.decile)

#Evaluate performance on test set
glm.test <- predict(glm.cc.final,newdata=cc_glm[test,],type="response")

glm.test.resp <- glm.test

ordered_data <- cbind(cc$DEFAULT[test],glm.test.resp)
colnames(ordered_data) <- c("label","pred")
ordered_data <- as.data.frame(ordered_data)
ordered_data <- ordered_data[order(ordered_data$label),]
roc(ordered_data$label,ordered_data$pred)

glm.test.resp[glm.test>= 0.2] <- 1
glm.test.resp[glm.test< 0.2] <- 0

sum(glm.test.resp==0 & cc[test,"DEFAULT"]==0)
sum(glm.test.resp==1 & cc[test,"DEFAULT"]==1)
sum(glm.test.resp==1 & cc[test,"DEFAULT"]==0)
sum(glm.test.resp==0 & cc[test,"DEFAULT"]==1)


my.df <- as.data.frame(cbind(glm.test,cc_glm[test,"DEFAULT"]));
colnames(my.df) <- c("model.score","response")

decile.pts <- quantile(my.df$model.score,
			probs=c(0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95));

my.df$model.decile <- cut(my.df$model.score,breaks=c(0,decile.pts,1),
			labels=rev(c('01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20'))
			);
table(my.df$model.decile)

ks.table <- as.data.frame(list(Y0=table(my.df$model.decile,my.df$response)[,1],
		Y1=table(my.df$model.decile,my.df$response)[,2],
		Decile=rev(c('01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20'))
		));

ks.table[order(ks.table$Decile),]

#Evaluate performance of logistic regression model on validation data
validation=which(cc$validate==1)

glm.validation <- predict(glm.cc.final,newdata=cc_glm[validation,],type="response")

glm.validation.resp <- glm.validation

ordered_data <- cbind(cc$DEFAULT[validation],glm.validation.resp)
colnames(ordered_data) <- c("label","pred")
ordered_data <- as.data.frame(ordered_data)
ordered_data <- ordered_data[order(ordered_data$label),]
roc(ordered_data$label,ordered_data$pred)

glm.validation.resp[glm.validation>= 0.2] <- 1
glm.validation.resp[glm.validation< 0.2] <- 0

sum(glm.validation.resp==0 & cc[validation,"DEFAULT"]==0)
sum(glm.validation.resp==1 & cc[validation,"DEFAULT"]==1)
sum(glm.validation.resp==1 & cc[validation,"DEFAULT"]==0)
sum(glm.validation.resp==0 & cc[validation,"DEFAULT"]==1)

#Create ROC curve and find K-S Statistic on validation set
my.df <- as.data.frame(cbind(glm.validation,cc_glm[validation,"DEFAULT"]));
colnames(my.df) <- c("model.score","response")

decile.pts <- quantile(my.df$model.score,
			probs=c(0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95));

my.df$model.decile <- cut(my.df$model.score,breaks=c(0,decile.pts,1),
			labels=rev(c('01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20'))
			);
table(my.df$model.decile)

ks.table <- as.data.frame(list(Y0=table(my.df$model.decile,my.df$response)[,1],
		Y1=table(my.df$model.decile,my.df$response)[,2],
		Decile=rev(c('01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20'))
		));

ks.table[order(ks.table$Decile),]


###############################
#Neural network model

#Modify variables for model-specific requirements
#Neural networks take numeric variables
cc_nn <- as.data.frame(apply(cc,2,as.numeric))
cc_nn$SEX[cc_nn$SEX==2] <- 0
cc_nn$EDUCATION[cc_nn$EDUCATION != 4] <- 0
cc_nn$EDUCATION[cc_nn$EDUCATION == 4] <- 1
cc_nn$MARRIAGE2 <- 0
cc_nn$MARRIAGE2[cc_nn$MARRIAGE == 2] <- 1
cc_nn$MARRIAGE3 <- 0
cc_nn$MARRIAGE3[cc_nn$MARRIAGE == 3] <- 1

#Scale variables for better neural network performance
max_data <- apply(cc_nn,2,max)
min_data <- apply(cc_nn,2,min)
cc_scale <- scale(cc_nn, center=min_data, scale=max_data-min_data)

#Build model
library(neuralnet)
set.seed(100)
cc.nn <- neuralnet(DEFAULT ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE2 + MARRIAGE3 + Age_18_25 + Age_26_40 + Avg_Bill_Amt + Avg_Pmt_Amt + Avg_Pmt_Ratio + Avg_Util + Bal_Growth_6mo + Util_Growth_6mo + Max_Bill_Amt + Max_Pmt_Amt + Max_DLQ,data=cc_scale[train,],threshold=0.3,linear.output=FALSE,hidden=7)

#Measure performance on training set
nn.train <- predict(cc.nn,newdata=cc_scale[train,])

nn.train.resp <- nn.train
nn.train.resp[nn.train>= 0.5] <- 1
nn.train.resp[nn.train< 0.5] <- 0

ordered_data <- cbind(cc_nn$DEFAULT[train],nn.train.resp)
colnames(ordered_data) <- c("label","pred")
ordered_data <- as.data.frame(ordered_data)
ordered_data <- ordered_data[order(ordered_data$label),]
roc(ordered_data$label,ordered_data$pred)

sum(nn.train.resp==0 & cc_nn[train,"DEFAULT"]==0)
sum(nn.train.resp==1 & cc_nn[train,"DEFAULT"]==1)
sum(nn.train.resp==1 & cc_nn[train,"DEFAULT"]==0)
sum(nn.train.resp==0 & cc_nn[train,"DEFAULT"]==1)

#Measure performance on test set
nn.test <- predict(cc.nn,newdata=cc_scale[test,])

nn.test.resp <- nn.test
nn.test.resp[nn.test>= 0.5] <- 1
nn.test.resp[nn.test< 0.5] <- 0

ordered_data <- cbind(cc_nn$DEFAULT[test],nn.test.resp)
colnames(ordered_data) <- c("label","pred")
ordered_data <- as.data.frame(ordered_data)
ordered_data <- ordered_data[order(ordered_data$label),]
roc(ordered_data$label,ordered_data$pred)

sum(nn.test.resp==0 & cc_nn[test,"DEFAULT"]==0)
sum(nn.test.resp==1 & cc_nn[test,"DEFAULT"]==1)
sum(nn.test.resp==1 & cc_nn[test,"DEFAULT"]==0)
sum(nn.test.resp==0 & cc_nn[test,"DEFAULT"]==1)

##################


