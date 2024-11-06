library(tidyverse)
library(ggpointdensity)
library(ggupset)
library(ggcorrplot)
library(shiny)
library(shinyalert)
library(DT)

my_sample_initial<-read_csv("Seoul_Bike_Data.csv")
my_sample<-my_sample_initial|>
  mutate(month=str_split_i(Date,'/',2),
         month=as.numeric(month),day=str_split_i(Date,'/',1),
         day=as.numeric(day))|>
  mutate(Seasonsnum=ifelse(Seasons=='Summer',1,ifelse(Seasons=='Autumn',2,ifelse(Seasons=='Winter', 3, 4))),
         Holidaynum=ifelse(Holiday=='No Holiday', 1, 0),
         `Functioning Day num`=ifelse(`Functioning Day`=='Yes',1,0))

numeric_vars<-c("Rented Bike Count", "Temperature(°C)", "Humidity(%)", "Wind speed (m/s)", "Visibility (10m)",
                "Dew point temperature(°C)", "Solar Radiation (MJ/m2)", "Rainfall(mm)", "Snowfall (cm)")
numeric_my_sample<-my_sample|>
  select(`Rented Bike Count`, `Temperature(°C)`, `Humidity(%)`, `Wind speed (m/s)`, `Visibility (10m)`,
         `Dew point temperature(°C)`, `Solar Radiation (MJ/m2)`, `Rainfall(mm)`, `Snowfall (cm)`)
categorical_my_sample<-my_sample|>
  select(Hour, Seasons, `Functioning Day`, Holiday, month)
# Read in data

Seasonsvals <- c(
  "1" = "Summer",
  "2" = "Autumn",
  "3" = "Winter",
  "4" = "Spring")

Holidayvals<-c("1"="No Holiday", '0'="Holiday")

Funcvals<-c("1" = "Yes", "2" = "No")












ui <- fluidPage(
  
  titlePanel("Seoul Bike Sharing Exploration"),
  sidebarLayout(
    sidebarPanel(
      h2("Choose the numeric subsets of the data:"),
      selectizeInput("corr_x",
                     "x Variable",
                     choices = names(numeric_my_sample)[-1], 
                     selected = names(numeric_my_sample)[2]),
      sliderInput("bins", "Number of bins:",
                  min = 1, max = 50, value = c(1,2)),
      selectizeInput("corr_y",
                     "y Variable",
                     choices = names(numeric_my_sample)[-2],
                     selected = names(numeric_my_sample)[1]),
      sliderInput("bins2", "Number of bins:",
                  min = 1, max = 50, value = c(1,2)),
      h2("Choose the categorical subset of the data:"),
      radioButtons("hhl_seasons",
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
      radioButtons("fs_func",
                   "Functioning",
                   choiceValues = c( 
                                    "Yes",
                                    "No"
                   ),
                   choiceNames = c(
                                   "Yes",
                                   "No"
                   )
      ),
      radioButtons("schl_holi",
                   "Holiday",
                   choiceValues = c(
                                    "Holiday",
                                    "No Holiday"
                   ),
                   choiceNames = c(
                                   "Yes",
                                   "No"
                   )
      ),
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
               
               actionButton("start_subset","Subset"))
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
    updateSliderInput(session, "bins", max = max(numeric_my_sample|>select(input$corr_x)),
                      min = min(numeric_my_sample|>select(input$corr_x)),
                      value = c(min(numeric_my_sample|>select(input$corr_x)),max(numeric_my_sample|>select(input$corr_x))))
  })
  observe({
    updateSliderInput(session, "bins2", max = max(numeric_my_sample|>select(input$corr_y)),
                      min = min(numeric_my_sample|>select(input$corr_y)),
                      value = c(min(numeric_my_sample|>select(input$corr_y)),max(numeric_my_sample|>select(input$corr_y))))
  })
  
  data<-reactiveValues(a=NULL)
  
  observeEvent(input$start_subset, {
    if(input$hhl_seasons == "All"){
      seasons_sub <- Seasonsvals
    } else if(input$hhl_seasons == "Summer"){
      seasons_sub <- Seasonsvals["1"]
    } else if(input$hhl_seasons == "Autumn"){
      seasons_sub <- Seasonsvals["2"]
    } else if(input$hhl_seasons == "Winter"){
      seasons_sub <- Seasonsvals["3"]
    }else {
      seasons_sub <- Seasonsvals["4"]
    }
    subset<-my_sample|>
      select(input$corr_y, input$corr_x, names(categorical_my_sample))|>
      filter(Holiday == input$schl_holi, Seasons %in% seasons_sub,
             `Functioning Day` == input$fs_func)
    
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

}

# Run the application 
shinyApp(ui = ui, server = server)



extra<-my_sample|>
  filter(Seasons == "Summer")
