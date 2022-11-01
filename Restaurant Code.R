library(shiny)
library(RMySQL)
library(DBI)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readxl)
library(janitor)
library(lubridate)
library(shinyWidgets)
library(shinyjs)
library(shinydashboard)
library(ggthemes)
library(shinythemes)
library(htmlwidgets)
library(viridis)
library(waiter)
library(highcharter) 
library(DT)
library(RSQLite)
library(shinyFeedback)    
library(qdapRegex)
library(imager)
library(shinymanager)
library(pool)
library(rhandsontable)
library(shinyalert)
library(officer)
library(glue)
library(sortable)
library(flextable)
library(shinyTime)
library(ggplot2)
library(flexdashboard)
library(readr)
library(timevis)
library(patchwork)
library(shinycssloaders)
library(gmailr)
makereactivetrigger <- function() {
  rv <- reactiveValues(a = 0)
  list(
    depend = function() {
      rv$a
      invisible()
    },
    trigger = function() {
      rv$a <- isolate(rv$a + 1)
    }
  )
}

dbtrigger <- makereactivetrigger()
con <- dbConnect(MySQL(), user = 'root', password = 'sri2003inmysql',
                 dbname = 'semester', host = 'localhost')

ui<-dashboardPage(
  
  dashboardHeader(title="RADISON BLUE RESTAURANT "),
  dashboardSidebar("          ",
                   width=150,
                   sidebarMenu(      
                     menuItem(h4("Home") , tabName = "Home", icon = icon("house-user")),
                     menuItem(h4("Menu-Card") , tabName = "Menu_Card", icon = icon("bars")),
                     menuItem(h4("Location"), tabName = "Location", icon = icon("map")),
                     menuItem(h4("Combo-Offers"), tabName = "Combo_offers", icon = icon("bahai")),
                     menuItem(h4("Orders"), tabName = "Orders", icon = icon("phone")),
                     menuItem(h4("Review"), tabName = "Review", icon = icon("star")),
                     menuItem(h4("Help & Support"), tabName = "Help", icon = icon("phone"))
                     
                     
                     
                   )),
  dashboardBody(
    
    setBackgroundColor(
      color = c("#F7FBFF", "#2171B5"),
      gradient = "linear",
      direction = "bottom"),
    
    
    tabItems(
      tabItem("Home",
              h1("Welcome to our website"),
              h2("Radison Blue Restaurant Best quality and taste"),
              h3("PLAY WITH YOUR FOOD!!!!!")
      ),
      tabItem("Menu_Card",
              fluidPage(
                div(p(h1("Taste good"))),
                div(p(h2("Select your dish"))),
                div(p(h2("Starters"))),
                div(p(actionLink("do", h3("Malaai-Cutlad------------Rs.80"),width=300))),
                div(p(actionLink("do", h3("Baby Corn Fries----------------Rs.190"),width=300))),
                div(p(actionLink("do", h3("Chicken 65----------Rs.150(8pcs)"),width=300))),
                div(p(actionLink("do", h3("Dragon Chicken-----------Rs.200"),width=300))),
                div(p(actionLink("do", h3("Chilli Chicken-------------Rs.250(8Pcs)"),width=300))),
                div(p(actionLink("do", h3("Triple 7-----------Rs.250"),width=300))),
                div(p(actionLink("do", h3("Mutton Chopps----------Rs.100"),width=300))),
                div(p(actionLink("do", h3("Prawn 65------------------Rs.120"),width=300))),
                div(p(actionLink("do", h3("Fish Fingers------------------Rs.220"),width=300))),
                div(p(h2("Main Course"))),
                div(p(actionLink("do", h3("Porucha Parota----------Rs.150(2Pcs)"),width=300))),
                div(p(actionLink("do", h3("Full Grill------------------Rs.320"),width=300))),
                div(p(actionLink("do", h3("Half Grill------------------Rs.160"),width=300))),
                div(p(actionLink("do", h3("Bun Parota------------------Rs.120"),width=300))),
                div(p(actionLink("do", h3("Chicken Parota------------------Rs.160"),width=300))),
                div(p(actionLink("do", h3("Chilli Parota------------------Rs.130"),width=300))),
                div(p(actionLink("do", h3("Egg Briyani------------------Rs.100"),width=300))),
                div(p(actionLink("do", h3("Mutton Briyani------------------Rs.250"),width=300))),
                div(p(actionLink("do", h3("Chicken Briyani------------------Rs.200"),width=300))),
                div(p(actionLink("do", h3("Chicken Noodels------------------Rs.120"),width=300))),
                div(p(actionLink("do", h3("Chicken Fried Rice------------------Rs.120"),width=300))),
                div(p(h2("Desserts"))),
                div(p(actionLink("do", h3("Faluda------------------Rs.100"),width=300))),
                div(p(actionLink("do", h3("Brownie Pastry------------------Rs.120"),width=300))),
                div(p(actionLink("do", h3("Nutella Brownie------------------Rs.108"),width=300))),
                div(p(actionLink("do", h3("Choco Lava Cake------------------Rs.150"),width=300))),
                div(p(actionLink("do", h3("Double Trouble------------------Rs.125"),width=300))),
              )
              
      ),
      
      
      tabItem("Location",
              
              fluidPage(
                numericInput('pincode','Pincode:', value = 1L, step = 1L),
                textInput('location_name','Location_name:'),
                textInput('district','District:'),
                actionButton('writetoloc', 'Save'),
                
              ),
      ),
      
      tabItem("Combo_offers",
              fluidPage(
                h1("Today Special"),
                h1("Pick your Combo here!!!!"),
                dataTableOutput("Offer"),
                div(p(actionLink("do", h4("Chicken Biriyani And Mutton Chops ------------- Rs.750"),width=300))),
                div(p(actionLink("do", h4("Shawarma and Hariyali Kebab ------------------- Rs.350"),width=300))),
                div(p(actionLink("do", h4("Mutton Biriyani And Chilli Chicken ------------ Rs.450"),width=300))),
                div(p(actionLink("do", h4("Porucha Parotta And Malaii Cutlet ------------- Rs.250"),width=300))),
                
              ),
      ),
      tabItem("Orders",
              fluidPage(
                h1("Place your Order here!!!!!!"),
                numericInput('order_id','Order_id:', value = 1L, step = 1L),
                textInput('Order_dish','Order_dish:'),
                numericInput('quantity','Quantity:', value = 1L, step = 1L),
                actionButton('writetoa', 'Save')
              ),
      ),
      tabItem("Review",
              fluidPage(
                h1("Throw your Review here!!!!!"),
                textInput('name','Name:'),
                textInput('comment','Comment:'),
                actionButton('writetodb','save'),
                tableOutput('dbtable'),
              ),
      ),
      tabItem("Help",
              fluidPage(
                
                h1("Help Line:"),
                h3("+91 8942521422"),
                h3("ngk0127@gmail.com"),
                h3(" 1)	         Police Control Room---------dial--100"),
                h3(" 2)	       Fire & Rescue Services----------dial--101"),
                h3(" 3)	             Ambulance-----------dial--108"),
                h3(icon = icon(""))
                
              ))
      
    )
  ))


server <- function(input, output) {
  observeEvent(input$writetodb, {
    sql2 = "INSERT INTO review VALUES (?name,?comment)"
    sql3 <- sqlInterpolate(con, sql2, name=input$name, comment=input$comment)
    dbExecute(con, sql3)
    dbtrigger$trigger()
  })
  observeEvent(input$writetoloc, {
    sql1 = "INSERT INTO location VALUES (?pincode,?location_name,?district)"
    sql <- sqlInterpolate(con, sql1, pincode=input$pincode, location_name=input$location_name, district=input$district)
    dbExecute(con, sql)
    dbtrigger$trigger()
  })
  observeEvent(input$writetoa, {
    sql4 = "insert into order1 values (?order_id,?Order_dish,?quantity)"
    sql5 <- sqlInterpolate(con, sql4, order_id=input$order_id,Order_dish=input$Order_dish,quantity=input$quantity)
    dbExecute(con, sql5)
    dbtrigger$trigger()
  })
  
}

shinyApp(ui, server)








library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
Dataset <- read_excel("semester.xlsx",n_max=420)
print(Dataset)
summary(Dataset)
year <- Dataset$`Day`
income <- Dataset$`Average Income per day`
expenses <- Dataset$`Expenses per day`
plot(Dataset$`Day`,Dataset$`Average Income per day`,type='l')
Time_data <- c(1,2,3,4,5,6,7)
Income_data <- c(95.5,63.8,72.4,75.5,75.6,89.9,99.3)
time_series <- ts(Income_data)
plot.ts(time_series)
result <- data.frame(Time_data,Income_data)
print(result)
f = Income_data/length(Income_data)
alpha = 2/(length(Income_data)+1)
F_7 = sum(Income_data)/7
cat("Forecast of day 7  : ",F_7)
Forecast = ((alpha)*99.3)+((1-alpha)*F_7)        #Ft+1 = alpha * yt + (1-alpha)*Ft
cat("   Forecast for the day 8 : ",Forecast)
x = c(1,2,3,4,5,6,7)
y = c(95.5,63.8,72.4,75.5,75.6,89.9,99.3)
dell_y=c(0)

dells_y=c(0,0)

len=0
for (i in lengths(y)){
  len=len+1
}
for (i in 2:len){
  temp=y[i]-y[i-1]
  
  dell_y=append(dell_y,temp)
}
for (i in 3:len){
  temp=dell_y[i]-dell_y[i-1]
  
  dells_y=append(dells_y,temp)
}

dell_y_by_y=dell_y/ y
print(y)
print(dell_y)
print(dells_y)
print(dell_y_by_y)
co_dell_y=mean(abs(mean(dell_y)-dell_y))
co_dells_y=mean(abs(mean(dells_y)-dells_y))
co_dell_y_by_y=mean(abs(mean(dell_y_by_y)-dell_y_by_y))
print(co_dell_y)
print(co_dells_y)
print(co_dell_y_by_y)
constant=min(c(co_dell_y,co_dells_y,co_dell_y_by_y))
print(constant)
med <- median(x)
cat("Med : ",med)
x = Time_data
print(x)
t=c()
Y=c()
num=readline(prompt="Enter the value of day to be calculated : ")
num=as.integer(num)
if(constant == co_dell_y_by_y){
  tsq=c()
  tY=c()
  for(i in 1:7){
    diff=x[i]-med             #t = x - median
    t=append(t,diff)
    i=i+1
  }
  for(i in 1:7){
    ca1 = log(y[i])
    Y=append(Y,ca1)
    i=i+1
  }
  for(i in length(x)){
    ca2 = t*t
    tsq=append(tsq,ca2)
    i=i+1
  }
  for(i in length(x)){
    ca3 = t*Y
    tY=append(tY,ca3)
    i=i+1
  }
  result1 <- data.frame(x,t,y,Y,tsq,tY)
  print(result1)
  summary(result1)
  A=sum(Y)/length(x)
  b=sum(tY)/sum(tsq)
  print(b)
  a = exp(A)
  print(a)
  cat("THE EXPONENTIAL FORM OF THE EQUATION :",a,"*", "e ^",b,"(x-4) ")       #y = a * e^bt
  c=a*exp(b*(num-4))
  cat("   The trend value for the entered day  ",num,"is:" ,c)
}

rest.data <- read_excel("semester.xlsx")
summary(rest.data)
one.way <- aov(`Average Income per day` ~ `Expenses per day`, data = rest.data)
summary(one.way)


df <- read_excel("semester.xlsx")
df

Income<- c(df$`Average Income per day`)
Income
Block <- c(df$Day)
Block
Budget <- c(df$`Expenses per day`)
Budget


new_df <- data.frame(Block,Income,Budget)
new_df

colnames(new_df) <- c('Year','x1','x2')

head(new_df)
tail(new_df)
library(readxl)
library(qicharts)
head(new_df[,2:3])

new_df$x.bar <- rowMeans(new_df[,2:3])
head(new_df)

qic(new_df$x.bar,chart = 'i',main = 'x bar chart',ylab = 'sample range', xlab = 'samples')

#c chart
library(qcc)
df <- read_excel("semester.xlsx")
q <- c(df$`Average Income per day`)
qcc(q,type ="c")


values = qcc.groups(df$`Average Income per day`,df$`Expenses per day`)

library(qcc)
qcc(values, type ="S", xlab="Expenses", ylab="Income")
q=df$`Expenses per day`

# Visualize
boxplot(df$`Average Income per day` ~ df$`Day`,
        col="light blue", ylab = "Income", xlab = "order")

#CUSUM....
rest.data <- read_excel("semester.xlsx")
Income= c(rest.data$`Average Income per day`)
Days = c(rest.data$`Day`)
cf <- c(rest.data$`CF`)
sum1=sum(Income,na.rm=FALSE)
sum1
newdata <- data.frame(Days,Income,cf)
num <- nrow(newdata)
num
central_line=sum1/num
central_line
ucl <- (central_line + 3*sqrt(central_line))
ucl
lcl <- (central_line - 3*sqrt(central_line))
newdata
csx <- cumsum(newdata$cf)
plot(x = newdata$Days,
     y = csx,
     main = "Cusum Chart",
     xlab = "day",
     ylab = "Cumulative Sum Score",
     type = "o")
abline(h = c(ucl,lcl) ,col = "black")




chi1 <- chisq.test(rest.data$`Average Income per day`)
chi2 <- chisq.test(rest.data$`Expenses per day`)
chi1
chi2


