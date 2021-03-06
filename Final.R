# install.packages("RPostgreSQL")
require("RPostgreSQL")
set.seed(0)
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
library(rpart.plot)
library(mlbench)
library(caret)

validation_index <- createDataPartition(bills$status, p=0.85, list=FALSE)
bills$party <- as.factor(bills$party)
bills$status <- as.factor(bills$status)
# select 20% of the data for validation
validation <- bills[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- bills[validation_index,]
control <- trainControl(method="repeatedcv", number=10, repeats=5)
#Final model choice
modelRpart <- train(status~no_days+party+ac_count+state, data=dataset, method="rpart", trControl=control)
predictions <- predict(modelRpart, validation)
confusionMatrix(predictions, validation$status)

plotcp(modelRpart)
printcp(modelRpart)

rpart.plot(modelRpart$finalModel)
prp(modelRpart$finalModel)
plot(modelRpart$finalModel,  uniform=TRUE, 
     main="Classification Tree for Kyphosis")
text(modelRpart$finalModel, use.n=TRUE, all=TRUE, cex=.8)

library(RColorBrewer)
library(rattle)
fancyRpartPlot(modelRpart$finalModel)


