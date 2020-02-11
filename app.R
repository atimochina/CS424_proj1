installed.packages("lubridate")
installed.packages("ggplot2")
installed.packages("leaflet")
installed.packages("stringr")
installed.packages("splitstackshape")

#libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(leaflet)
library(stringr)
library(splitstackshape)
library(DT)

#Accessing data from data folder
data <- read.table('./data/litterati challenge-65.csv',sep=',',header=TRUE,stringsAsFactors = FALSE)

#----------------------Data Cleaning----------------------------------
#removing unnecessary data components for analysis
#Only need the userid, username, tags, time and locations of litter
data$url <- NULL #removing url
data$challengeId <- NULL #removing challenge id
data$litterjoinId <- NULL
data$litterId <- NULL

#Cleaning tags column, empty cells become untagged
data$tags[data$tags == ""] <- "untagged" 

#Changing latitude and longitude to numeric value
data$lat <- as.numeric(data$lat)
data$lon <- as.numeric(data$lon)

#Removing zero values
data$lat[data$lat == 0] <- NA
data$lon[data$lon == 0] <- NA
data <- na.omit(data)

#Removing outliers
outliersLat <- boxplot(data$lat)$out
outliersLon <- boxplot(data$lon)$out
data$lat[data$lat %in% outliersLat]<-NA
data$lon[data$lon %in% outliersLon]<-NA
data <- na.omit(data)

#Creating a vector of potential bad usernames
badUsernames <- paste("litterati-",data$user_id,sep="")

#Using list of bad usernames to find the poorly named users
#changing to more understandable format of user_userid
for(i in 1:length(data$username)) {
  if(badUsernames[i] == data$username[i]){
    data$username[i] <- paste("user_",data$user_id[i],sep="")}
}

#Cleaning date/time format
time <- ymd_hms(data$litterTimestamp)
time <- force_tz(time, "GMT") #Setting as GMT
time <- with_tz(time, "America/Chicago") #Converting to Chicago time
data$litterTimestamp <- time #fixing data timestamp

#Separating tags
data$tags <- str_to_lower(data$tags)#tags to lower case
data$tags <- str_replace(data$tags, " ", "")#removing spaces

#Seperating each tag entry into a new row with same info
#litterTimeStamp, lat, lon, individual tag, user_id, username
data <- cSplit(data, "tags", sep=",", direction = "long")
data$tags <- as.character(data$tags)

#making sure data is in correct format
data$lat <- as.numeric(as.character(data$lat))
data$lon <- as.numeric(as.character(data$lon))
data$username <- as.character(data$username)
#--------------------------------------------------------------------

#creating array of Count info to get top ten
tags1 <- data$tags
users <- data$username

#list of all tags that are unique
list_of_tags <- unique(tags1)
#list of usernames that are unique
list_of_users <- unique(users)

tagInfo <- as.data.frame(table(tags1), stringsAsFactors = FALSE)
userInfo <- as.data.frame(table(users),stringsAsFactors = FALSE)

tagInfo <- tagInfo[order(tagInfo$Freq, decreasing = TRUE),]
userInfo <- userInfo[order(userInfo$Freq, decreasing = TRUE), ]

#creating a top ten data table for users and tags
top_ten_tags <- tagInfo[1:10, 1:2]
top_ten_users <- userInfo[1:10, 1:2]

#list of days/hours
list_of_days <- seq(as.Date(mdy("April 4, 2018")), as.Date(mdy("January 7, 2020")), by="days")
list_of_hours <- c(0:24)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "CS 424 Project 1: Angy Timochina", titleWidth = 400),
    #Drop down options
    dashboardSidebar(disable = FALSE, collapsed = FALSE,
                     br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(), #breaks
                     br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(), #breaks
                     selectInput("User", "Top Ten Users", c("summary",top_ten_users[,1]),selected = "summary"),
                     br(),br(),br(),br(),
                     selectInput("TagSelect", "Top Ten Tag", c("none",top_ten_tags[,1]), selected = "none"),
                     br(),
                     actionButton("show", "Open Resources"),
                     actionButton("close", "Close Resources")
    ),
    #main body
    dashboardBody(
      fluidRow(
        column(12,fluidRow(box(title = "# of Litter per Day",solidHeader = TRUE, status = "primary", width = 12, plotOutput("bar1",height = 400))))
      ),
      fluidRow(
        column(2,fluidRow(box(title = "# of Litter per Day Table",solidHeader = TRUE, status = "primary", width = 12, dataTableOutput("table1",height = 400)))
        ),
        column(2,fluidRow(box(title = "# of Litter per Weekday Table",solidHeader = TRUE, status = "primary", width = 12, dataTableOutput("table2",height = 400)))
        ),
        column(2,fluidRow(box(title = "# of Litter per Hour Table",solidHeader = TRUE, status = "primary", width = 12, dataTableOutput("table3",height = 400)))
        ),
        column(2,fluidRow(box(title = "# of Litter per Top Tag Table",solidHeader = TRUE, status = "primary", width = 12, dataTableOutput("table4",height = 400)))
        ),
        column(1,fluidRow(box(title = "Total Number of Litter Picked-Up: ", solidHeader = TRUE, status = "primary", width = 12, textOutput("total"))),
               fluidRow(box(title = "Top Ten Users", solidHeader = TRUE,status = "primary", width = 12, dataTableOutput("tableUser", height = 300)))
        ),
        column(3,fluidRow(box(title = "Leaflet Map",solidHeader = TRUE, status = "primary", width = 12, leafletOutput("leaf",height = 400)))
        )
      ),
      fluidRow(
        column(4,fluidRow(box(title = "# of Litter per Weekday",solidHeader = TRUE, status = "primary", width = 12, plotOutput("bar2",height = 400)))
        ),
        column(4,fluidRow(box(title = "# of Litter per Hour",solidHeader = TRUE, status = "primary", width = 12, plotOutput("bar3",height = 400)))
        ),
        column(4,fluidRow(box(title = "# of Litter per Top Tag",solidHeader = TRUE, status = "primary", width = 12, plotOutput("bar4",height = 400)))
        ),
    ))
)

server <- function(input, output) {
  # increase the default font size
  theme_set(theme_grey(base_size = 18)) 

  dataReactive <- reactive({
    if(input$User == "summary" & input$TagSelect == "none"){
      #no change to overall data
      return(data)
    }
    else if(input$User == "summary"){
      #no change to user options, but tag changed
      return(data[data$tags == input$TagSelect])
    }
    else if(input$TagSelect == "none"){
      #no change to tag options but change to user options
      return(data[data$username == input$User])
    }
    else {
      return(data[data$username == input$User & data$tags == input$TagSelect])
    }
  })
  output$bar1 <- renderPlot({
    newData <- dataReactive()
    
    #Days with litter frequency
    date <- date(newData$litterTimestamp)
    litter_date <- as.data.frame(table(date))
    
    ggplot(litter_date, aes(x= date, y=Freq))+geom_bar(stat="identity", fill="steelblue") +
      labs(x="Day", y = "Litter Count") + theme(axis.text.x=element_text(angle=90, hjust=1))
  })
  output$table1 <- DT::renderDataTable(
    DT::datatable({
      newData <- dataReactive()
      #Days with litter frequency
      date <- date(newData$litterTimestamp)
      litter_date <- as.data.frame(table(date))
    },
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    )
  )
  output$bar2 <- renderPlot({
    newData <- dataReactive()
    
    #days of week litter frequency
    day_of_week <- weekdays(newData$litterTimestamp)
    litter_day_of_week <- as.data.frame(table(day_of_week))
    litter_day_of_week$day_of_week <- factor(litter_day_of_week$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    
    ggplot(litter_day_of_week, aes(x= day_of_week, y=Freq))+geom_bar(stat="identity", fill="steelblue") +
      labs(x="Day of Week", y = "Litter Count")  
  })
  output$table2 <- DT::renderDataTable(
    DT::datatable({
      newData <- dataReactive()
      #days of week litter frequency
      day_of_week <- weekdays(newData$litterTimestamp)
      litter_day_of_week <- as.data.frame(table(day_of_week))
      },
      options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
      ), rownames = FALSE 
    )
  )
  output$bar3 <- renderPlot({
    newData <- dataReactive()
    
    #Hour with litter frequency
    hour <- hour(data$litterTimestamp)
    litter_hour <- as.data.frame(table(hour))

    ggplot(litter_hour, aes(x= hour, y=Freq))+geom_bar(stat="identity", fill="steelblue") +
      labs(x="Hour of the Day", y = "Litter Count")
  })
  output$table3 <- DT::renderDataTable(
    DT::datatable({
      newData <- dataReactive()
      #Hour with litter frequency
      hour <- hour(data$litterTimestamp)
      litter_hour <- as.data.frame(table(hour))
    },
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    )
  )
  output$bar4 <- renderPlot({
    newData <- dataReactive()
    
    #frequency of top ten tags according to user
    tagList <- top_ten_tags[,1]
    newData2 <- newData[newData$tags %in% tagList]
    
    tagFreq <- as.data.frame(table(newData2$tags))
    
    ggplot(tagFreq, aes(x = Var1, y=Freq))+ geom_bar(stat="identity", fill="steelblue") +
      labs(x="Tag", y = "Litter Count")
  })
  output$table4 <- DT::renderDataTable(
    DT::datatable({
      newData <- dataReactive()
      #frequency of top ten tags according to user
      tagList <- top_ten_tags[,1]
      newData2 <- newData[newData$tags %in% tagList]
      
      tagFreq <- as.data.frame(table(newData2$tags))
    },
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    )
  )
  output$tableUser <- DT::renderDataTable(
    cleanTable <- as.data.frame(top_ten_users, row.names = c(1:length(top_ten_users[,1]))),
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
  )
  output$leaf <- renderLeaflet({
    newData <- dataReactive()
    
    map <- leaflet()
    map <- addTiles(map)
    map <- setView(map, lng = mean(newData$lon), lat = mean(newData$lat), zoom = 14)
    map <- addMarkers(map, lng = newData$lon , lat = newData$lat, popup = newData$tags,clusterOptions = markerClusterOptions())
    map
  })
  output$total <- renderText(
    {paste(length(data$tags))}
  ) 
  
  #------------------------------Creating notification popup for resources-------
  #https://shiny.rstudio.com/reference/shiny/1.4.0/showNotification.html
  #used above site as reference
  
  # A queue of notification IDs
  ids <- character(0)
  # A counter
  n <- 0
  
  observeEvent(input$show, {
    # Save the ID for removal later
    id <- showNotification(
      paste(
        "Basis of code from Prof.Andrew Johnsons Example at...",
        "** https://www.evl.uic.edu/aej/424/week02.html",
        "Resources...", 
        "** https://www.rdocumentation.org/packages/splitstackshape/versions/1.4.8/topics/cSplit",
        "** https://cran.r-project.org/web/packages/stringr/stringr.pdf", 
        "** https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/as.data.frame",
        "** https://www.datamentor.io/r-programming/data-frame/",
        "** https://rstudio.github.io/leaflet/markers.html",
        "** https://stackoverflow.com/questions/50253615/rendering-datatable-in-shiny",
        "** https://rstudio.github.io/DT/shiny.html",
        "** https://shiny.rstudio.com/reference/shiny/0.14/renderDataTable.html",
        "** https://shiny.rstudio.com/articles/datatables.html",
        "** https://stackoverflow.com/questions/56310163/reactive-bar-chart-in-shiny-r",
        "** https://stackoverflow.com/questions/21515800/subset-a-data-frame-based-on-user-input-shiny",
        "** https://www.rdocumentation.org/packages/lessR/versions/3.7.6/topics/Subset",
        "** https://www.statmethods.net/management/subset.html",
        "** http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization",
        "** https://stackoverflow.com/questions/40199274/how-to-force-specific-order-of-the-variables-on-the-x-axis",
        "** https://shiny.rstudio.com/articles/reactivity-overview.html",
        "** https://shiny.rstudio.com/tutorial/written-tutorial/lesson6/",
        "** https://stackoverflow.com/questions/56310163/reactive-bar-chart-in-shiny-r",
        "** https://shiny.rstudio.com/images/shiny-cheatsheet.pdf",
        "** https://stackoverflow.com/questions/14450384/create-a-vector-of-all-days-between-two-dates",
        "** https://rawgit.com/rstudio/cheatsheets/master/lubridate.pdf",
        "** https://lubridate.tidyverse.org/",
        "** https://www.dummies.com/programming/r/how-to-work-with-dates-in-r/",
        "** https://rpubs.com/Mentors_Ubiqum/removing_outliers",
        "** https://shiny.rstudio.com/reference/shiny/latest/fluidPage.html",
        "...Libraries used...",
        "** shiny, shinydashboard, ggplot2, lubridate, leaflet, stringr, splitstackshape, DT"
        ), 
      duration = NULL)
    ids <<- c(ids, id)
    n <<- n + 1
  })
  
  observeEvent(input$close, {
    if (length(ids) > 0)
      removeNotification(ids[1])
    ids <<- ids[-1]
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
