library(plotly)
require("RPostgreSQL")
library(plotly)



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


sql_command<- "SELECT   c.name,   to_char(a.action_date, 'YYYY-MM') dt,   count(*) cnt FROM actions a, committees c WHERE a.committee = c.id       AND a.type = 'IntroReferral'       AND c.name IN (   SELECT name   FROM (          SELECT            c.name,            count(*) cnt          FROM actions a, committees c          WHERE a.committee = c.id                AND a.type = 'IntroReferral'          GROUP BY c.name          ORDER BY cnt DESC          LIMIT 5) tb) GROUP BY   c.name, to_char(a.action_date, 'YYYY-MM') order by dt , cnt desc"

bills<-dbGetQuery(con, sql_command)
bills<- bills[,c(2,1,3)]
cr <- xtabs(cnt~.,bills)


cr$dt<- rownames(cr)
cr <- as.data.frame.matrix(cr)
p <- plot_ly(cr, x = ~dt, y = ~cr[,1], name = colnames(cr)[1], type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~cr[,2], name = colnames(cr)[2], mode = 'lines') %>%
  add_trace(y = ~cr[,3], name = colnames(cr)[3], mode = 'lines') %>%
  add_trace(y = ~cr[,4], name = colnames(cr)[4], mode = 'lines') %>%
  add_trace(y = ~cr[,5], name = colnames(cr)[5], mode = 'lines')
p

