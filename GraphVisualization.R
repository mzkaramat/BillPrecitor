#install.packages("GGally")
library(GGally)

devtools::install_github("briatte/ggnet")
library(ggnet)

library(network)
library(sna)
library(ggplot2)

require("RPostgreSQL")








pw <- {
  "1ifGTLdq1PkY"
}

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(
  drv,
  dbname = "congress",
  host = "192.168.128.193",
  port = 5431,
  user = "user11",
  password = pw
)


sql_command <-
  "SELECT   type,next_type,   avg(next_row - action_date) no_days FROM (        SELECT          bill_id,          action_date,          type,          lead(action_date, 1)          OVER (            PARTITION BY bill_id            ORDER BY action_date ) next_row,          lead(type, 1)          OVER (            PARTITION BY bill_id            ORDER BY action_date ) next_type        FROM actions        ORDER BY bill_id, action_date      ) tb1   INNER JOIN bills     ON bills.id = tb1.bill_id   where next_type is not NULL group by type,next_type ;"
#sql_command <- "SELECT         type,         avg(             CASE WHEN type = 'BecameLaw'               THEN                 next_row - action_date             ELSE 0 END) became_law,         avg(             CASE WHEN type = 'Calendars'               THEN                 next_row - action_date             ELSE 0 END) Calendars,         avg(             CASE WHEN type = 'Committee'               THEN                 next_row - action_date             ELSE 0 END) Committee,         avg(             CASE WHEN type = 'Discharge'               THEN                 next_row - action_date             ELSE 0 END) Discharge,         avg(             CASE WHEN type = 'Floor'               THEN                 next_row - action_date             ELSE 0 END) Floor,         avg(             CASE WHEN type = 'IntroReferral'               THEN                 next_row - action_date             ELSE 0 END) IntroReferral,         avg(             CASE WHEN type = 'NotUsed'               THEN                 next_row - action_date             ELSE 0 END) NotUsed,         avg(             CASE WHEN type = 'President'               THEN                 next_row - action_date             ELSE 0 END) President,         avg(             CASE WHEN type = 'ResolvingDifferences'               THEN                 next_row - action_date             ELSE 0 END) ResolvingDifferences,         avg(             CASE WHEN type = 'Veto'               THEN                 next_row - action_date             ELSE 0 END) Veto       FROM (SELECT               bill_id,               action_date,               type,               lead(action_date, 1)               OVER (                 PARTITION BY bill_id                 ORDER BY action_date ) next_row,               lead(type, 1)               OVER (                 PARTITION BY bill_id                 ORDER BY action_date ) next_type             FROM actions             ORDER BY bill_id, action_date) tb1 INNER JOIN bills ON bills.id = tb1.bill_id       WHERE next_type IS NOT NULL       GROUP BY type"
bills_inp <- dbGetQuery(con, sql_command)
#bills_inp <- bills_inp[,-1]
#nodes<-rownames(bills_inp)

v <- as.data.frame(nodes)
names(v)
e <- bills_inp[, -3]
names(e)

net = network(e,
              , directed = TRUE)
ggnet2(
  net,
  directed = TRUE,
  arrow.size = 12,
  arrow.gap = 0.025,
  color = "grey",
  label = TRUE,
  alpha = 0.75,
  size = 10,
  edge.alpha = 0.5
)
