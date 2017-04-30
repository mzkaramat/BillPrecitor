install.packages("GGally")
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
con <- dbConnect(drv, dbname = "congress",
                 host = "192.168.128.193", port = 5431,
                 user = "user11", password = pw)


sql_command<- "SELECT   type,next_type,   avg(next_row - action_date) no_days FROM (        SELECT          bill_id,          action_date,          type,          lead(action_date, 1)          OVER (            PARTITION BY bill_id            ORDER BY action_date ) next_row,          lead(type, 1)          OVER (            PARTITION BY bill_id            ORDER BY action_date ) next_type        FROM actions        ORDER BY bill_id, action_date      ) tb1   INNER JOIN bills     ON bills.id = tb1.bill_id   where next_type is not NULL group by type,next_type ;"
bills_inp<-dbGetQuery(con, sql_command)
nodes<-unique(c(bills_inp[,1],bills_inp[,2]))
v<- as.data.frame(nodes)
names(v)
e<- bills_inp[,c(1,2)]
names(e)

net = network(e, directed = TRUE)
x = data.frame(Twitter = network.vertex.names(net))
x = merge(x, v, by = "Twitter", sort = FALSE)$Groupe
net %v% "party" = as.character(x)

y = RColorBrewer::brewer.pal(9, "Set1")[ c(3, 1, 9, 6, 8, 5, 2) ]
names(y) = levels(x)

net = rgraph(10, mode = "graph", tprob = 0.5)
net = network(net, directed = FALSE)

ggnet2(net, color = "orange", label = TRUE,
       alpha = 0.75, size = 10, edge.alpha = 0.5
       )

# vertex names
network.vertex.names(net) = letters[1:10]
ggnet2(net)
