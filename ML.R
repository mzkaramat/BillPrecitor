# install.packages("RPostgreSQL")
require("RPostgreSQL")

# create a connection
# save the password that we can "hide" it as best as we can by collapsing it
pw <- {
  "G7iFeMJnkukW"
}

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "congress",
                 host = "192.168.128.193", port = 5431,
                 user = "user04", password = pw)


sql_command<- " SELECT     tb1.bill_id,     bills.bill_type,     bills.originchamber,     to_char(bills.createdate, 'DD/MM/YYYY') createdate,    to_char( bills.updatedate, 'DD/MM/YYYY') updatedate,     to_char(bills.introduceddate, 'DD/MM/YYYY') introduceddate,    EXTRACT(DAY FROM bills.updatedate - bills.createdate) no_days,     members.party,     members.state,     bill_count_tbl.ac_count,     CASE WHEN upper(text_string) LIKE upper('%status%') or upper(text_string) LIKE upper('%BecameLaw%')       THEN 'Passed'      ELSE 'REFERRED' END status   FROM (SELECT           bill_id,           string_agg(status, ' ') text_string         FROM (SELECT                 bill_id,                 action_date,                 type                        status,                 max(action_date)                 OVER                   (                   PARTITION BY bill_id ) AS max_thing               FROM actions) tb         WHERE tb.max_thing = tb.action_date         GROUP BY bill_id) tb1 INNER JOIN bills ON bills.id = tb1.bill_id     INNER JOIN (SELECT                   count(*) ac_count,                   bill_id                 FROM actions                 GROUP BY bill_id) bill_count_tbl       ON bill_count_tbl.bill_id = tb1.bill_id     inner join sponsors     on sponsors.bill_id = tb1.bill_id   inner join members     on members.bioguideid = sponsors.sponsor_id"

bills<-dbGetQuery(con, sql_command)

nrow(bills)

colnames(bills)

summary(bills)

library(rpart)

fit <- rpart(status~bill_type+no_days+party+ac_count,
             method="class", data=bills)
printcp(fit)
plotcp(fit)
summary(fit)

library(mlbench)
library(caret)




#colnames(bills) <- c("Bill Id", "Bill Type","originchamber","createdate","updatedate","introduceddate"
#                     "No of Days","Party Type","state","Actions count","status"
#                    )
# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(bills$status, p=0.85, list=FALSE)
# select 20% of the data for validation
validation <- bills[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- bills[validation_index,]





control <- trainControl(method="repeatedcv", number=10, repeats=5)
modelLvq <- train(status~no_days+party+ac_count, data=dataset, method="lvq", trControl=control)

modelGbm <- train(status~no_days+party+ac_count, data=dataset, method="gbm", trControl=control, verbose=FALSE)


modelSvm <- train(status~no_days+party+ac_count, data=dataset, method="svmRadial", trControl=control)


#Final model choice
modelRpart <- train(status~no_days+party+ac_count+state, data=dataset, method="rpart", trControl=control)
predictions <- predict(modelRpart, validation)
confusionMatrix(predictions, validation$status)

plotcp(modelRpart)
printcp(modelRpart)

rpart.plot(modelRpart$finalModel)
prp(modelRpart$finalModel)
plot(modelRpart$finalModel, uniform=TRUE, 
     main="Classification Tree for Kyphosis")
text(modelRpart$finalModel, use.n=TRUE, all=TRUE, cex=.8)

library(RColorBrewer)
library(rattle)
fancyRpartPlot(modelRpart$finalModel)


results <- resamples(list(LVQ=modelLvq, GBM=modelGbm, SVM=modelSvm,modelRpart))

summary(results)
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)


#C50
predictions <- predict(modelRpart, validation)
confusionMatrix(predictions, validation$status)

predictions <- predict(modelLvq, validation)
confusionMatrix(predictions, validation$status)


predictions <- predict(modelGbm, validation)
confusionMatrix(predictions, validation$status)

predictions <- predict(modelSvm, validation)
confusionMatrix(predictions, validation$status)

install.packages("C50")
library(C50)
dataset$status <- as.factor(dataset$status)
model <- C5.0(status~no_days+party+ac_count, data = dataset)
results <- predict(object = model, newdata = validation, type = "class")
table(results, validation$status)
confusionMatrix(results, validation$status)


# Now Kmeans
library(e1071)
library(printr)
model <- svm(status~no_days+party+ac_count, data = dataset)

results <- predict(object = model, newdata = validation, type = "class")

table(results, validation$status)
confusionMatrix(results, validation$status)



#Now using adaboost
library(adabag)
model <- boosting(status~no_days+party+ac_count, data = dataset)
results <- predict(object = model, newdata = validation, type = "class")
results$confusion


#Using KNN
model <- naiveBayes(x = subset(dataset, select=-status), y = dataset$status)
results <- predict(object = model, newdata = validation, type = "class")
table(results, validation$status)
confusionMatrix(results, validation$status)


#Using Rpart
model <- rpart(status~no_days+party+ac_count, data = dataset)
results <- predict(object = model, newdata = validation, type = "class")
confusionMatrix(results, validation$status)
printcp(model)
plotcp(model)
plot(model, uniform=TRUE, 
     main="Classification Tree for Kyphosis")
text(model, use.n=TRUE, all=TRUE, cex=.8)
library(rpart.plot)
rpart.plot(model)

rm(list = ls(all = TRUE)) 
gc(reset=TRUE)
set.seed(1234) #From random.org

#Libraries
library(caret)
library(devtools)
install_github('caretEnsemble', 'zachmayer') #Install zach's caretEnsemble package
library(caretEnsemble)

#Data
library(mlbench)
dat <- mlbench.xor(500, 2)
X <- data.frame(dat$x)
Y <- factor(ifelse(dat$classes=='1', 'Yes', 'No'))


#Split train/test
train <- runif(nrow(X)) <= .66

#Setup CV Folds
#returnData=FALSE saves some space
folds=5
repeats=1
myControl <- trainControl(method='cv', number=folds, repeats=repeats, 
                          returnResamp='none', classProbs=TRUE,
                          returnData=FALSE, savePredictions=TRUE, 
                          verboseIter=TRUE, allowParallel=TRUE,
                          summaryFunction=twoClassSummary,
                          index=createMultiFolds(Y[train], k=folds, times=repeats))
PP <- c('center', 'scale')

#Train some models
model1 <- train(X[train,], Y[train], method='gbm', trControl=myControl,
                tuneGrid=expand.grid(.n.trees=500, .interaction.depth=15, .shrinkage = 0.01))
model2 <- train(X[train,], Y[train], method='blackboost', trControl=myControl)
model3 <- train(X[train,], Y[train], method='parRF', trControl=myControl)
model4 <- train(X[train,], Y[train], method='mlpWeightDecay', trControl=myControl, trace=FALSE, preProcess=PP)
model5 <- train(X[train,], Y[train], method='knn', trControl=myControl, preProcess=PP)
model6 <- train(X[train,], Y[train], method='earth', trControl=myControl, preProcess=PP)
model7 <- train(X[train,], Y[train], method='glm', trControl=myControl, preProcess=PP)
model8 <- train(X[train,], Y[train], method='svmRadial', trControl=myControl, preProcess=PP)
model9 <- train(X[train,], Y[train], method='gam', trControl=myControl, preProcess=PP)
model10 <- train(X[train,], Y[train], method='glmnet', trControl=myControl, preProcess=PP)

#Make a list of all the models
all.models <- list(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10)
names(all.models) <- sapply(all.models, function(x) x$method)
sort(sapply(all.models, function(x) min(x$results$ROC)))

#Make a greedy ensemble - currently can only use RMSE
greedy <- caretEnsemble(all.models, iter=1000L)
sort(greedy$weights, decreasing=TRUE)
greedy$error

#Make a linear regression ensemble
linear <- caretStack(all.models, method='glm', trControl=trainControl(method='cv'))
linear$error

#Predict for test set:
library(caTools)
preds <- data.frame(sapply(all.models, function(x){predict(x, X[!train,], type='prob')[,2]}))
preds$ENS_greedy <- predict(greedy, newdata=X[!train,])
preds$ENS_linear <- predict(linear, newdata=X[!train,], type='prob')[,2]
sort(data.frame(colAUC(preds, Y[!train])))