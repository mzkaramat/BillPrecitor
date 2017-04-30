## app.R ##
library(shinydashboard)

library(shiny)
require("RPostgreSQL")

library(rpart)
library(rpart.plot)
library(mlbench)
library(caret)
set.seed(0)
library(GGally)
library(plotly)

devtools::install_github("briatte/ggnet")
library(ggnet)

library(network)
library(sna)
library(ggplot2)


# create a connection
# save the password that we can "hide" it as best as we can by collapsing it
pw <- {
  "G7iFeMJnkukW"
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
  user = "user04",
  password = pw
)



sql_command<- "SELECT   c.name,   to_char(a.action_date, 'YYYY-MM') dt,   count(*) cnt FROM actions a, committees c WHERE a.committee = c.id       AND a.type = 'IntroReferral'       AND c.name IN (   SELECT name   FROM (          SELECT            c.name,            count(*) cnt          FROM actions a, committees c          WHERE a.committee = c.id                AND a.type = 'IntroReferral'          GROUP BY c.name          ORDER BY cnt DESC          LIMIT 5) tb) GROUP BY   c.name, to_char(a.action_date, 'YYYY-MM') order by dt , cnt desc"

bills<-dbGetQuery(con, sql_command)
bills<- bills[,c(2,1,3)]
cr <- xtabs(cnt~.,bills)

cr <- as.data.frame.matrix(cr)
print("cr <- as.data.frame.matrix(cr)")
cr$dt<- rownames(cr)





sql_command <-
  " SELECT     tb1.bill_id,     bills.bill_type,     bills.originchamber,     to_char(bills.createdate, 'DD/MM/YYYY') createdate,    to_char( bills.updatedate, 'DD/MM/YYYY') updatedate,     to_char(bills.introduceddate, 'DD/MM/YYYY') introduceddate,    EXTRACT(DAY FROM bills.updatedate - bills.createdate) no_days,     members.party,     members.state,     bill_count_tbl.ac_count,     CASE WHEN upper(text_string) LIKE upper('%status%') or upper(text_string) LIKE upper('%BecameLaw%')       THEN 'Passed'      ELSE 'REFERRED' END status   FROM (SELECT           bill_id,           string_agg(status, ' ') text_string         FROM (SELECT                 bill_id,                 action_date,                 type                        status,                 max(action_date)                 OVER                   (                   PARTITION BY bill_id ) AS max_thing               FROM actions) tb         WHERE tb.max_thing = tb.action_date         GROUP BY bill_id) tb1 INNER JOIN bills ON bills.id = tb1.bill_id     INNER JOIN (SELECT                   count(*) ac_count,                   bill_id                 FROM actions                 GROUP BY bill_id) bill_count_tbl       ON bill_count_tbl.bill_id = tb1.bill_id     inner join sponsors     on sponsors.bill_id = tb1.bill_id   inner join members     on members.bioguideid = sponsors.sponsor_id"


#sql_command <-"select bill_id,count(*) cnt from actions group by bill_id"
bills_inp <- dbGetQuery(con, sql_command)


bills <-
  bills_inp[, c("no_days", "party", "ac_count", "state", "status")]


set.seed(0)
validation_index <-
  createDataPartition(bills$status, p = 0.85, list = FALSE)
bills$party <- as.factor(bills$party)
bills$status <- as.factor(bills$status)
# select 20% of the data for validation
validation <- bills[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- bills[validation_index,]
control <- trainControl(method = "repeatedcv",
                        number = 10,
                        repeats = 5)
#Final model choice
modelRpart <-
  train(
    status ~ no_days + party + ac_count + state,
    data = dataset,
    method = "rpart",
    trControl = control
  )





ui <- dashboardPage(
  dashboardHeader(title = "Hackathon"),
  ## Sidebar content
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Introduction",
      tabName = "tb_introduction",
      icon = icon("dashboard")
    ),
    menuItem("Process Analysis", tabName = "tb_analysis", icon = icon("th")),
    menuItem("Bill timelines", tabName = "tb_time_analyzer", icon = icon("th")),
    menuItem("Bill Predictor", tabName = "tb_bil_pred", icon = icon("th")), 
    menuItem("Title generator", tabName = "tb_title_gen", icon = icon("th"))
  )),
  dashboardBody(tabItems(
    # First tab content
    tabItem(tabName = "tb_introduction",
            
            
            # infoBoxes with fill=TRUE
            fluidRow(
              infoBox("Most number of actions", 261, icon = icon("credit-card")),
              infoBox("Most time taking bill", 785, icon = icon("credit-card")),
              infoBox("The leader", "?name?", icon = icon("credit-card"))
            ),
            
            fluidRow(
              # Clicking this will increment the progress amount
              box(
                title = "Histogram1", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("plot4", height = 250)
              ),
              
              box(
                title = "Histogram2", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("plot5", height = 250)
              )
            ),
            
            fluidRow(
              # Clicking this will increment the progress amount
              box(
                title = "Histogram3", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("plot6", height = 250)
              ),
              
              box(
                title = "Histogram4", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("plot7", height = 250)
              )
            )),
    
    # Second tab content
    tabItem(tabName = "tb_analysis",
            sidebarLayout(
              sidebarPanel(
                h3("Process analyzer for the bill "),
                radioButtons(
                  "party_type_net",
                  "Party type",
                  choices = c("Republican", "Democrats"),
                  selected = "Republican"
                ), br(),
                radioButtons(
                  "bill_type_net",
                  "Origin Chamber",
                  choices = c("Senate", "House"),
                  selected = "Senate"
                ),br(),selectInput("state_net", "State",
                                   choices = sort(unique(bills$state)))
              ),
              
              mainPanel(
                box(
                  title = "Histogram", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,width = 550,height = 650,
                plotOutput("network_plot", height = 600,width = 500)) )
            )),
    
    tabItem(tabName = "tb_time_analyzer",
            
            
            
            
            
            
            
            sidebarLayout(
              sidebarPanel(
                h3("Time series analysis of top commities"),
                radioButtons(
                  "year_cmt",
                  "Choose year ",
                  choices = c("2015", "2016"),
                  selected = "2015"
                ),br(),sliderInput("month_cmt", "Choose month:",
                                   min=0, max=12,  value = c(2,10))
              ),
              
              mainPanel(
                box(
                  title = "Time Series analysis", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,width = 550,height = 650,
                  plotlyOutput("time_seri_cmt", height = "600px",width = "100%")) )
            )
            
            
            
            
            
            
            
            
            
            
            ),
    
    # Second tab content
    tabItem(
      tabName = "tb_bil_pred",
      titlePanel("US Congress bills predictor"),
      sidebarLayout(
        sidebarPanel(
          h3(
            "This predictor will predict the bill success rate based on important factors"
          ),
          br(),
          h5(" No of days are count of days from the bill creation date"),
          br(),
          h5(" Action count is the count of actions on the bill"),
          br(),
          h5(" Party type will be either Republican or Democrat"),
          br(),
          h5(" And the state will be state on which the bill is presented")
        ),
        
        mainPanel(
          sliderInput(
            "no_of_days",
            "Days between",
            0,
            800,
            c(500),
            post = " days",
            step = 25
          ),
          sliderInput("ac_count", "Actions count", 0, 50, c(20), post = " actions"),
          radioButtons(
            "party_type",
            "Party type",
            choices = c("Republican", "Democrats"),
            selected = "Republican"
          ),
          
          selectInput("state", "State",
                      choices = sort(unique(bills$state))),
          br(),
          h4(textOutput("text1"))
        )
      )
      
    ), 
    
    # Second tab content
    tabItem(tabName = "tb_title_gen",
           
            h2("GNA/LSTM generator")
            
            
            )
  ))
)


server <- function(input, output) {
  set.seed(122)
  print("at start")
 
  histdata <- rnorm(500)
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  
  
  observeEvent(
    c(input$party_type_net,input$state_net,input$bill_type_net),
    {
      if (input$party_type_net == "Republican")
        filter_pred <-  'R'
      else
        filter_pred <-'D'
      
      print(paste("Slider moved ",filter_pred))
      sql_command = paste("SELECT   type,   next_type,   avg(next_row - action_date) no_days FROM (SELECT         bill_id,         action_date,         type,         lead(action_date, 1)         OVER (           PARTITION BY bill_id           ORDER BY action_date ) next_row,         lead(type, 1)         OVER (           PARTITION BY bill_id           ORDER BY action_date ) next_type       FROM actions       ORDER BY bill_id, action_date) tb1 INNER JOIN bills ON bills.id = tb1.bill_id   inner join sponsors on sponsors.bill_id = bills.id   inner join members on members.bioguideid = sponsors.sponsor_id WHERE next_type IS NOT NULL and members.party = '",filter_pred,"' and members.state = '", input$state_net,"' and bills.originchamber = '",input$bill_type_net,"' GROUP BY type, next_type;",sep = '')
      #sql_command <-
      # "SELECT   type,next_type,   avg(next_row - action_date) no_days FROM (        SELECT          bill_id,          action_date,          type,          lead(action_date, 1)          OVER (            PARTITION BY bill_id            ORDER BY action_date ) next_row,          lead(type, 1)          OVER (            PARTITION BY bill_id            ORDER BY action_date ) next_type        FROM actions        ORDER BY bill_id, action_date      ) tb1   INNER JOIN bills     ON bills.id = tb1.bill_id   where next_type is not NULL group by type,next_type ;"
      #sql_command <- "SELECT         type,         avg(             CASE WHEN type = 'BecameLaw'               THEN                 next_row - action_date             ELSE 0 END) became_law,         avg(             CASE WHEN type = 'Calendars'               THEN                 next_row - action_date             ELSE 0 END) Calendars,         avg(             CASE WHEN type = 'Committee'               THEN                 next_row - action_date             ELSE 0 END) Committee,         avg(             CASE WHEN type = 'Discharge'               THEN                 next_row - action_date             ELSE 0 END) Discharge,         avg(             CASE WHEN type = 'Floor'               THEN                 next_row - action_date             ELSE 0 END) Floor,         avg(             CASE WHEN type = 'IntroReferral'               THEN                 next_row - action_date             ELSE 0 END) IntroReferral,         avg(             CASE WHEN type = 'NotUsed'               THEN                 next_row - action_date             ELSE 0 END) NotUsed,         avg(             CASE WHEN type = 'President'               THEN                 next_row - action_date             ELSE 0 END) President,         avg(             CASE WHEN type = 'ResolvingDifferences'               THEN                 next_row - action_date             ELSE 0 END) ResolvingDifferences,         avg(             CASE WHEN type = 'Veto'               THEN                 next_row - action_date             ELSE 0 END) Veto       FROM (SELECT               bill_id,               action_date,               type,               lead(action_date, 1)               OVER (                 PARTITION BY bill_id                 ORDER BY action_date ) next_row,               lead(type, 1)               OVER (                 PARTITION BY bill_id                 ORDER BY action_date ) next_type             FROM actions             ORDER BY bill_id, action_date) tb1 INNER JOIN bills ON bills.id = tb1.bill_id       WHERE next_type IS NOT NULL       GROUP BY type"
      print(sql_command)
      bills_inp <- dbGetQuery(con, sql_command)
      print(nrow(bills_inp))
      if (nrow(bills_inp) != 0){
      #bills_inp <- bills_inp[,-1]
      nodes<-rownames(bills_inp)
     # if (nrow(bills_inp) != 0 )then{
       
        v <- as.data.frame(nodes)
        names(v)
        e <- bills_inp[, -3]
        names(e)
        
        net = network(e,
                      directed = TRUE)
        
        
        
        
        
        
        
        output$network_plot <-renderPlot( ggnet2(
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
        )
      }else{
        showNotification("No data found.",duration = 3)
        }
      
    })
  
  
  
  
  
  
  
  
  
  observeEvent(c(
    input$year_cmt,
    input$month_cmt
  ),
  {
    # myslider is a reactive but it does not trigger the code to
    # run here because we're using observeEvent and only specified
    # input$mytext
    print (input$month_cmt)
    sql_command<-paste("SELECT   c.name,   to_char(a.action_date, 'YYYY-MM') dt,   count(*)                          cnt FROM actions a, committees c WHERE   to_char(a.action_date, 'YYYY') = '",input$year_cmt,"'   and to_char(a.action_date, 'MM') >= LPAD('",input$month_cmt[1],"',2,'0')     and to_char(a.action_date, 'MM')  <= LPAD('",input$month_cmt[2],"',2,'0') and   a.committee = c.id AND a.type = 'IntroReferral' AND c.name IN (SELECT name                                                                      FROM (SELECT                                                                              c.name,                                                                              count(*) cnt                                                                            FROM actions a, committees c                                                                            WHERE a.committee = c.id AND                                                                                  a.type = 'IntroReferral'                                                                            GROUP BY c.name                                                                            ORDER BY cnt DESC                                                                            LIMIT 5) tb) GROUP BY c.name, to_char(a.action_date, 'YYYY-MM') ORDER BY dt, cnt DESC",sep = '')
    print(sql_command)
    bills<-dbGetQuery(con, sql_command)
    bills<- bills[,c(2,1,3)]
    cr <- xtabs(cnt~.,bills)
    cr <- as.data.frame.matrix(cr)
   
    cr["dt"]<- rownames(cr)
    
    p <- plot_ly(cr, x = ~dt, y = ~cr[,1], name = colnames(cr)[1], type = 'scatter', mode = 'lines') %>%
      add_trace(y = ~cr[,2], name = colnames(cr)[2], mode = 'lines') %>%
      add_trace(y = ~cr[,3], name = colnames(cr)[3], mode = 'lines') %>%
      add_trace(y = ~cr[,4], name = colnames(cr)[4], mode = 'lines') %>%
      add_trace(y = ~cr[,5], name = colnames(cr)[5], mode = 'lines')
    print("I am almost there")
   output$time_seri_cmt <- renderPlotly(p)
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  observeEvent(c(
    input$no_of_days,
    input$party_type,
    input$ac_count,
    input$state
  ),
  {
    # myslider is a reactive but it does not trigger the code to
    # run here because we're using observeEvent and only specified
    # input$mytext
    
    no_days = c(input$no_of_days)
    party = c(if (input$party_type == "Republican")
      'R'
      else
        'D')
    ac_count = c(input$ac_count)
    state = c(input$state)
    record <-
      data.frame(c(no_days), c(party), c(ac_count), c(state))
    #print (record)
    outp <-
      predict(modelRpart, data.frame(no_days, party, ac_count, state))
    outp <- paste('Bill will ', outp)
    #print(outp)
    print("changed")
    output$text1 <- renderText({
      outp
    })
    
  })

  output$progressBox <- renderInfoBox({
    infoBox(
      "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
      color = "purple"
    )
  })
  
}

shinyApp(ui, server)