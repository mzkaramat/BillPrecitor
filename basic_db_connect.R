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
rm(pw) # removes the password

# check for the cartable
bills<-dbExistsTable(con, "bills")
# TRUE

sql_command<-'select * from bills'

bills<-dbGetQuery(con, sql_command)

str(bills)

class(bills)

structure(bills)

summary(bills)

head(bills)

factor(bills$originchamber)

sql_command <- 'select bill_id,text,count(*) from actions where action_date = (select max(A.action_date) from actions A where A.action_code = actions.action_code) group by bill_id,text;'

bills_status = dbGetQuery(con,sql_command)

