library(tidyverse)
library(ggpointdensity)
library(ggupset)
library(ggcorrplot)
library(shiny)
library(shinyalert)
library(DT)

my_sample<-read_csv("Seoul_Bike_Data.csv")
my_sample<-my_sample|>
  filter(`Functioning Day`== 'Yes')|>
  mutate(month=str_split_i(Date,'/',2),
         month=as.numeric(month),day=str_split_i(Date,'/',1),
         day=as.numeric(day))|>
  mutate(Seasonsnum=ifelse(Seasons=='Summer',1,ifelse(Seasons=='Autumn',2,ifelse(Seasons=='Winter', 3, 4))),
         Holidaynum=ifelse(Holiday=='No Holiday', 1, 0),
         `Functioning Day num`=ifelse(`Functioning Day`=='Yes',1,0))|>
  rename('Rented_Bike_Count'=`Rented Bike Count`, "Temperature" = `Temperature(°C)`, "Rainfall"=`Rainfall(mm)`, 
         "Snowfall" = `Snowfall (cm)`, "Humidity" = `Humidity(%)`, "Wind_speed" = `Wind speed (m/s)`,
         "Visability" = `Visibility (10m)`, "Dew_point_temperature" = `Dew point temperature(°C)`,
         "Solar_radiation" = `Solar Radiation (MJ/m2)`)

numeric_my_sample<-my_sample|>
  select(Rented_Bike_Count, Temperature, Humidity, Wind_speed, Visability,
         Dew_point_temperature, Solar_radiation, Rainfall, Snowfall)
categorical_my_sample<-my_sample|>
  select(Hour, Seasons, Holiday)
# Read in data

Seasonsvals <- c(
  "1" = "Summer",
  "2" = "Autumn",
  "3" = "Winter",
  "4" = "Spring")

Holidayvals<-c("1"="No Holiday", '0'="Holiday")

Hourvals<-c('0',"1","2","3","4","5","6","7","8","9","10","11","12","13","14",
            "15", "16", "17", "18", "19", "20", "21", "22", "23")

ui <- fluidPage(
  
  titlePanel("Seoul Bike Sharing Exploration"),
  sidebarLayout(
    sidebarPanel(
      h2("Choose the numeric subsets of the data:"),
      selectizeInput("x",
                     "X Variable",
                     choices = names(numeric_my_sample)[-1], 
                     selected = names(numeric_my_sample)[2]),
      sliderInput("bins", "Range of x",
                  min = 1, max = 50, value = c(1,2)),
      selectizeInput("y",
                     "Y Variable",
                     choices = names(numeric_my_sample)[-2],
                     selected = names(numeric_my_sample)[1]),
      sliderInput("bins2", "Range of y",
                  min = 1, max = 50, value = c(1,2)),
      h2("Choose the categorical subset of the data:"),
      radioButtons("seasons",
                   "Seasons",
                   choiceValues = c( "All",
                                    "Summer",
                                    "Autumn",
                                    "Spring",
                                    "Winter"
                   ),
                   choiceNames = c("All",
                                   "Summer",
                                   "Autumn",
                                   "Spring",
                                   "Winter"
                   )
      ),
      selectizeInput("hour",
                     "Hour",
                     choices = c("All", 0:23),
                     selected = 0),
      radioButtons("holi",
                   "Holiday",
                   choiceValues = c("All",
                                    "Holiday",
                                    "No Holiday"
                   ),
                   choiceNames = c("All",
                                   "Yes",
                                   "No"
                   )
      ),
      actionButton("start_subset","Subset"),
    ),
    mainPanel(
      tabsetPanel(id="tabs",
      tabPanel("About",textOutput("about"),
      uiOutput("url"),
      textOutput("about2"),
      imageOutput("bike")),
      tabPanel("Data Download",
               DT::dataTableOutput("mytable"),downloadButton("downloadData", "Download")),
      tabPanel("Data Exploration",
               radioButtons("Facet_wrap",
                            "Facet_wrap",
                            choiceValues = c("Hour",
                                             "Holiday",
                                             "Seasons"
                            ),
                            choiceNames = c("Hour",
                                            "Holiday",
                                            "Seasons"
                            )
               )
               ,
               radioButtons("Color",
                            "Color",
                            choiceValues = c("Hour",
                                             "Holiday",
                                             "Seasons"
                            ),
                            choiceNames = c("Hour",
                                            "Holiday",
                                            "Seasons"
                            )
               ),
               plotOutput("corr_scatter"),
               plotOutput("x_hist"),
               plotOutput("y_hist"),
               plotOutput("scatter_total")
               )
    )
  )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$about <- renderText({
    "This app was created to explore the Seoul Bike Sharing Demand Prediction data set. The data, from Kaggle,
    was collected in order to try to predict how weather affects the number of bikes used by a bike sharing 
    company in Seoul, South Korea. The data set has 14 variables, containing the number of bikes rented during a 
    particular hour on a particular day and information on the weather at the time and other circumstances. The 
    link to the source of the data is below."
  })

  output$url<-renderUI({
    tagList("",a("Seoul Bike Sharing Data", href="https://www.kaggle.com/datasets/saurabhshahane/seoul-bike-sharing-demand-prediction/data") )
  })
  output$about2 <- renderText({
    "In this app, a user can explore associations between numeric variables, subset the data by categorical
    and numeric variables, and download the subsetted data."
  })
  output$bike<-renderImage({ 
    filename <- normalizePath(file.path('./images','Bike.png'))
    
    list(src = filename,
         contentType = 'image/png',
         alt = "")
  }, deleteFile = FALSE)

  
  observe({
    updateSliderInput(session, "bins", max = max(numeric_my_sample|>select(input$x)),
                      min = min(numeric_my_sample|>select(input$x)),
                      value = c(min(numeric_my_sample|>select(input$x)),max(numeric_my_sample|>select(input$x))))
  })
  observe({
    updateSliderInput(session, "bins2", max = max(numeric_my_sample|>select(input$y)),
                      min = min(numeric_my_sample|>select(input$y)),
                      value = c(min(numeric_my_sample|>select(input$y)),max(numeric_my_sample|>select(input$y))))
  })
  
  data<-reactiveValues(a=NULL)
  
  observeEvent(input$start_subset, {
    if(input$seasons == "All"){
      seasons_sub <- Seasonsvals
    } else if(input$seasons == "Summer"){
      seasons_sub <- Seasonsvals["1"]
    } else if(input$seasons == "Autumn"){
      seasons_sub <- Seasonsvals["2"]
    } else if(input$seasons == "Winter"){
      seasons_sub <- Seasonsvals["3"]
    }else {
      seasons_sub <- Seasonsvals["4"]
    }
    if(input$hour == "All"){
      hour_sub <- Hourvals
    } else {
      hour_sub <- input$hour
    }
    if(input$holi == "All"){
      hol_sub <- Holidayvals
    } else if(input$holi == "Yes"){
      hol_sub <- Holidayvals["1"]
    } else {
      hol_sub <- Holidayvals["0"]
    }

    subset<-my_sample|>
      select(input$y, input$x, names(categorical_my_sample))|>
      filter(Holiday %in% hol_sub, Seasons %in% seasons_sub, Hour %in% hour_sub)
    
    data$a<-subset
  })
  
  
  output$mytable <- DT::renderDataTable({
    DT::datatable(data$a)
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("bike", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data$a, file, row.names = FALSE)
    }
  )

  output$corr_scatter<-renderPlot({
    ggplot(data$a, aes_string(x = isolate(input$x), y = isolate(input$y))) +
      geom_point(aes(color = get(input$Color)))+
      facet_wrap(~get(input$Facet_wrap))
  })
  output$x_hist<-renderPlot({
    ggplot(data$a, aes_string(x = isolate(input$x))) +
      geom_histogram()
  })
  output$y_hist<-renderPlot({
    ggplot(data$a, aes_string(x = isolate(input$y))) +
      geom_histogram()
  })
  output$scatter_total<-renderPlot({
    ggplot(data$a, aes_string(x = isolate(input$x), y = isolate(input$y))) +
      geom_point()
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
