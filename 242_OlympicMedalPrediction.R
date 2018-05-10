# Soumya Gupta, Rijul Mediratta, Scott Perkins
# IEOR 242
# Final Project

# Load Packages
install.packages("caret", dependencies = T)
install.packages("gbm")
library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)
library(gbm)
library(caTools)
library(dplyr)
library(ggplot2)
library(GGally)
library(ROCR)
library(MASS)

# Load cleaned data
medals = read.csv("Final_Raw_Data_v2_model.csv")

set.seed(123)

OSR2 <- function(predictions, train, test) {
  SSE <- sum((test - predictions)^2)
  SST <- sum((test - mean(train))^2)
  r2 <- 1 - SSE/SST
  return(r2)
}

# Training and test split
spl = sample.split(medals$Medal, SplitRatio = 0.7)
medals.train = medals %>% filter(spl == TRUE)
medals.test = medals %>% filter(spl == FALSE)

# Baseline model - assume average medals won
base.pred = round(mean(medals$Medals), digits=0)
base.osr2 = OSR2(base.pred, medals.train$Medals, medals.test$Medals)

# Linear model
medals.LM = lm(Medals~., data=medals.train)
LM.pred = predict(medals.LM, newdata=medals.test)
LM.osr2 = OSR2(LM.pred, medals.train$Medal, medals.test$Medal)

# CART model
medals.CART = train(Medals~., data=medals.train, method="rpart",
                    tuneGrid=data.frame(cp=seq(0,0.1,0.005)),
                    trControl = trainControl(method="cv", number=5),
                    metric = "RMSE")
medals.CART$results
medals.CART
ggplot(medals.CART$results, aes(x = cp, y = RMSE)) + geom_point(size = 3) + 
  ylab("RMSE") + theme_bw() + theme(axis.title=element_text(size=18), axis.text=element_text(size=18))

medals.CART.CV = medals.CART$finalModel
CART.pred = predict(medals.CART.CV, newdata = medals.test)
CART.osr2 = OSR2(CART.pred, medals.train$Medal, medals.test$Medal)

# Base Random Forest model
medals.RF = randomForest(Medals~., data=medals.train)
RF.pred = predict(medals.RF, newdata = medals.test)
RF.osr2 = OSR2(RF.pred, medals.train$Medal, medals.test$Medal)
factor.importance <- importance(medals.RF)
write.csv(factor.importance, file="importance")

# RF Cross validation
folds <- capture.output(train.RF <- train(Medals ~ .,
                                           data = medals.train,
                                           method = "rf",
                                           tuneGrid = data.frame(mtry=1:25),
                                           trControl = trainControl(method="cv", number=5, verboseIter = TRUE),
                                           metric = "RMSE"))
train.RF$results
train.RF
ggplot(train.RF$results, aes(x = mtry, y = RMSE)) + geom_point(size = 3) + 
  ylab("RMSE") + theme_bw() + theme(axis.title=element_text(size=18), axis.text=element_text(size=18))

# Random forest with tuned mtry=25
medals.RF.CV <- randomForest(Medals~., mtry=25, data=medals.train)
RF.CV.pred = predict(medals.RF.CV, newdata = medals.test)
RF.CV.osr2 = OSR2(RF.CV.pred, medals.train$Medal, medals.test$Medal)

factor.importance.CV <- importance(medals.RF.CV)
write.csv(factor.importance.CV, file="importance.CV")

# Boosting model
tGrid = expand.grid(n.trees = (1:100)*40, interaction.depth = c(1,2,4,6,8,10,12,14,16),
                    shrinkage = 0.01, n.minobsinnode = 10)

medals.boost = gbm(Medals ~ .,
                     data = medals.train,
                     distribution = "gaussian",
                     n.trees=20000,
                     interaction.depth=10)
medals.boost
summary(medals.boost)

#medals.boost$results
ggplot(medals.boost$results, aes(x = n.trees, y = RMSE, colour = as.factor(interaction.depth))) + geom_line() + 
  ylab("RMSE") + theme_bw() + theme(axis.title=element_text(size=18), axis.text=element_text(size=18)) + 
  scale_color_discrete(name = "interaction.depth")
#medals.boost.CV = medals.boost$finalModel

boost.pred = predict(medals.boost, newdata = medals.test, n.trees = 20000)
boost.osr2 = OSR2(boost.pred,medals.train$Medal, medals.test$Medal)
