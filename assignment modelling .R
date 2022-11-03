library(VIM)
library(ggplot2)
library(corrplot)
library(pROC)
library(caret)

# read data
data <- read.csv("Desktop/data.csv")

#data exploration
summary(data)

# case、weather-> factors
data$case <- factor(data$case,levels = c(0,1),labels = c("No traffic accidents",
                                                         "traffic accidents"))
data$Weather <- factor(data$Weather,levels = c(0,1),labels = c("Sunny","Rain"))
str(data1)

#the number of missing data
sum(is.na(data))

#the distribution of missing data
aggr(data, prop=FALSE, numbers = TRUE)

# delete missing data
nrow(data)
data1 <- data[!(is.na(data$case) |
                 is.na(data$spd_dif_1min) |
                 is.na(data$spd_dif_2min) |
                 is.na(data$spd_dif_3min) |
                 is.na(data$spd_dif_4min) |
                 is.na(data$vol_dif_1min) |
                 is.na(data$vol_dif_2min) |
                 is.na(data$vol_dif_3min) |
                 is.na(data$vol_dif_4min) |
                 is.na(data$Weather) ),]
nrow(data1)

#check for outliers for every variables
boxplot(data$case)
boxplot(data$spd_dif_1min)
boxplot(data$spd_dif_2min)
boxplot(data$spd_dif_3min)
boxplot(data$spd_dif_4min)
boxplot(data$vol_dif_1min)
boxplot(data$vol_dif_2min)
boxplot(data$vol_dif_3min)
boxplot(data$vol_dif_4min)
boxplot(data$Weather)
# no outliners

# Correlation analysis multiple variables
cor1 <- cor(data1[,2:9])
corrplot(cor1, method = "number")

#partitioning of the data
train <- sample(nrow(data1), 0.7*nrow(data1))

df.train <- data1[train,]
df.test <- data1[-train,]

#logistic model full fit
logistic1 <- glm(case ~., data = df.train, family = binomial)
summary(logistic1)

#logistic model reduced fit
logistic2 <- glm(case ~spd_dif_1min + spd_dif_4min + 
                   vol_dif_1min + vol_dif_2min, 
                 data = df.train, family = binomial)
summary(logistic2)

#chi square
anova(logistic1, logistic2, test = "Chisq")

#explain coefficients
exp(coef(logistic2))
exp(confint(logistic2))

# now plot the data
predicted.data <- data.frame(probability.of.case=logistic2$fitted.values,
                             x=-2.38+0.11*data1$spd_dif_1min
                             +0.1*data1$spd_dif_4min+0.02*data1$vol_dif_1min)

#ROC curve for trainning dataset
pre <- predict(logistic2, type='response',data1)

modelroc <- roc(data1$case,pre)
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE,
     grid=c(0.1, 0.2), grid.col=c("green", "red"),
     max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)

# prediction
predict <- predict(logistic2,type='response',newdata=df.test)
predict.results <- ifelse( predict> 0.125,
                           "traffic accidents",
                           "No traffic accidents")

# accuracy
misClasificError <- mean(predict.results != df.test$case)
print(paste('accuracy',1-misClasificError))

# ROC curve for test dataset
validate_pre <- predict(logistic2,type='response',df.test)
validate_roc <- roc(df.test$case,validate_pre)
plot(validate_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)
# AUC = 0.765, TPR = 0.833, TNR = 0.536
