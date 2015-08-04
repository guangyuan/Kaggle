train <- read.csv(file="/Users/guangyuanyang/Downloads/inspection/train.csv",header = T,fill = T)
test <- read.csv(file="/Users/guangyuanyang/Downloads/inspection/test.csv",header = T,fill = T)

target <- "Hazard"
idvar <- "Id"
predvar <- setdiff(names(train),c(target,idvar))



histogram(train[,target])
histogram(log(train[,target]))
table(train[,target])

library(gbm)
m1_gbm <- gbm(Hazard~.,distribution = "poisson", data = train,
              n.trees = 300,
              shrinkage = 0.1,
              interaction.depth = 2,
              train.fraction = 0.7,
              cv.folds = 5)

gbm.perf(m1_gbm)
varimp <- summary(m1_gbm)


par(mfrow=c(3,2))
for(i in 1:nrow(varimp)){
  (varname <- as.character(varimp[i,1]))
  plot.gbm(m1_gbm,i.var = varname)
}
par(mfrow=c(1,1))

test_pred <- predict.gbm(object = m1_gbm,n.trees = 157,type = "response",newdata = test)
train_pred <- predict.gbm(object = m1_gbm,n.trees = 157,type = "response",newdata = train)
NormalizedGini(train$Hazard,train_pred)

#######################
#TDboost

library(TDboost)
m1_tdboost <- TDboost(Hazard~.,distribution = list(name="EDM",alpha=2), data = train,
              n.trees = 2000,
              shrinkage = 0.01,
              interaction.depth = 2,
              train.fraction = 0.7)

best.iter <- TDboost.perf(m1_tdboost,method = "test")
varimp <- summary(m1_tdboost)


par(mfrow=c(3,2))
for(i in 1:nrow(varimp)){
  (varname <- as.character(varimp[i,1]))
  plot.TDboost(m1_tdboost,i.var = varname,n.trees = best.iter)
}
par(mfrow=c(1,1))

test_pred <- predict.TDboost(object = m1_tdboost,n.trees = best.iter,type = "response",newdata = test)
train_pred <- predict.TDboost(object = m1_tdboost,n.trees = best.iter,type = "response",newdata = train)
NormalizedGini(train$Hazard,train_pred)



write.table(data.frame(Id=test[,"Id"],Hazard=test_pred),row.names = FALSE,quote = FALSE,sep = ",",
            file = "/Users/guangyuanyang/Downloads/inspection/pred.csv")


#####################################
# GBDT




