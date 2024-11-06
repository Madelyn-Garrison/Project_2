###
# title: "558 Project 2"
# author: "Madelyn Garrison"
# format: html
# editor: visual
###

# Load required packages 

library(tidyverse)
library(ggpointdensity)
library(ggupset)
library(ggcorrplot)
library(shiny)
library(shinyalert)
library(DT)
library(plotly)
library(shinycssloaders)

# Read in data and clean
# Renaming to eliminate characters R shiny can't handle

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

# All numeric variables and their correlations + all categorical correlations

numeric_my_sample<-my_sample|>
  select(Rented_Bike_Count, Temperature, Humidity, Wind_speed, Visability,
         Dew_point_temperature, Solar_radiation, Rainfall, Snowfall)

corr<-cor(numeric_my_sample)

categorical_my_sample<-my_sample|>
  select(Hour, Seasons, Holiday)

# Formats for categorical variables

Seasonsvals <- c("1" = "Summer", "2" = "Autumn", "3" = "Winter", "4" = "Spring")

Holidayvals<-c("1"="No Holiday", '0'="Holiday")

Hourvals<-c('0',"1","2","3","4","5","6","7","8","9","10","11","12","13","14",
            "15", "16", "17", "18", "19", "20", "21", "22", "23")

## User Interface

ui <- fluidPage(
  titlePanel("Seoul Bike Sharing Exploration"),
  sidebarLayout(
    sidebarPanel(
      h2("Choose the numeric subsets of the data:"), 
      # A widget with selections to choose a numeric variable and the corresponding ranges
      selectizeInput("x", "X Variable",
                     choices = names(numeric_my_sample)[-1], 
                     selected = names(numeric_my_sample)[2]),
      sliderInput("bins", "Range of x",
                  min = 1, max = 50, value = c(1,2)),
      # Repeat for a second numeric variable
      selectizeInput("y", "Y Variable",
                     choices = names(numeric_my_sample)[-2],
                     selected = names(numeric_my_sample)[1]),
      sliderInput("bins2", "Range of y",
                  min = 1, max = 50, value = c(1,2)),
      h2("Choose the categorical subset of the data:"),
      # Choose a subset for the three categorical variables
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
      # Button to subset the data
      actionButton("start_subset","Subset"),
    ),
    mainPanel(
      tabsetPanel(id="tabs",
        # First tab is the about section for the app and the data set
        tabPanel("About",textOutput("about"),
          uiOutput("url"),
          textOutput("about2"),
          imageOutput("bike")),
        # Second tab is to display the subsetted data set and allow for it to be downloaded
        tabPanel("Data Download",
          DT::dataTableOutput("mytable"),downloadButton("downloadData", "Download")),
        # Third tab creates visuals and summaries
        # Additional widgets for graph aesthetics and specifying numeric summaries
        tabPanel("Data Exploration",
          radioButtons("Facet_wrap",
                        "Panel",
                        choiceValues = c("Seasons",
                                          "Holiday",
                                          "Hour"
                        ),
                        choiceNames = c("Seasons",
                                        "Holiday",
                                        "Hour"
                        )
          ),
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
          shinycssloaders::withSpinner(plotOutput("corr_scatter")),
          plotOutput("x_hist"),
          plotOutput("y_hist"),
          plotOutput("density"),
          checkboxGroupInput("num_var",
                              "Choose numeric variable",
                              choiceValues = c("1",
                                                "2"
                              ),
                              choiceNames = c("X variable",
                                              "Y variable"
                              )
          ),
          radioButtons("Function",
                        "Choose function",
                        choiceValues = c("mean",
                                          "median",
                                          "sd"
                        ),
                        choiceNames = c("Mean",
                                        "Median",
                                        "Standard Deviation"
                        )
          ),
          tableOutput("x_table"),
          checkboxGroupInput("cats", "Choose categorical variable(s)",
                              choiceValues = c("1", "2", "3"),
                              choiceNames = c("Hour", "Seasons", "Holiday")),
          tableOutput("y_table"),
          plotOutput("Corr")
        )
      )
    )
  )
)

## Server

server <- function(input, output, session) {
  
  # Output for the About tab
  
  output$about <- renderText({
    "This app was created to explore the Seoul Bike Sharing Demand Prediction data set. The data, from Kaggle,
    was collected in order to try to predict how weather affects the number of bikes used by a bike sharing 
    company in Seoul, South Korea. The data set has 14 variables, containing the number of bikes rented during a 
    particular hour on a particular day and information on the weather at the time and other circumstances. The 
    link to the source of the data is below."
  })
  
    # Creates hyperlink to data set source
  
  output$url<-renderUI({
    tagList("",a("Seoul Bike Sharing Data", 
            href="https://www.kaggle.com/datasets/saurabhshahane/seoul-bike-sharing-demand-prediction/data") )
  })
  
  output$about2 <- renderText({
    "In this app, a user can explore associations between numeric variables, subset the data by categorical
    and numeric variables, and download the subsetted data."
  })
  
    # Embeds image 
  
  output$bike<-renderImage({ 
    filename <- normalizePath(file.path('./images','Bike.png'))
    list(src = filename,
         contentType = 'image/png',
         alt = "")
  }, deleteFile = FALSE)

  # Update numeric variable sliders, using the minimum and maximum of the selected variable
  
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
  
  # Subset the data based on the user-selected variables and apply needed alterations
  
  data<-reactiveValues(a=NULL, num=NULL, cat=NULL)
  
  observeEvent(input$start_subset, {
    
    if (input$x == input$y){
      shinyalert(title = "Numeric variables must be different.", type = "error")
    } else {}
    
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

    vars<-c(input$x, input$y)
    
    subset<-my_sample|>
      select(input$x, input$y, names(categorical_my_sample))|>
      filter(Holiday %in% hol_sub, Seasons %in% seasons_sub, Hour %in% hour_sub)|>
      mutate(Hour = as.character(Hour))%>%
      {if("Temperature" %in% input$x) filter (.,Temperature >= input$bins[1] & Temperature <= input$bins[2]) else .} %>%
      {if("Rented_Bike_Count" %in% input$x) filter(.,Rented_Bike_Count >= input$bins[1] & Rented_Bike_Count <= input$bins[2]) else .} %>%
      {if("Humidity" %in% input$x) filter(.,Humidity >= input$bins[1] & Humidity <= input$bins[2]) else .} %>%
      {if("Wind_speed" %in% input$x) filter(.,Wind_speed >= input$bins[1] & Wind_speed <= input$bins[2]) else .} %>%
      {if("Visability" %in% input$x) filter(.,Visability >= input$bins[1] & Visability <= input$bins[2]) else .} %>%
      {if("Dew_point_temperature" %in% input$x) filter(.,Dew_point_temperature >= input$bins[1] & Dew_point_temperature <= input$bins[2]) else .} %>%
      {if("Solar_radiation" %in% input$x) filter(.,Solar_radiation >= input$bins[1] & Solar_radiation <= input$bins[2]) else .} %>%
      {if("Rainfall" %in% input$x) filter(.,Rainfall >= input$bins[1] & Rainfall <= input$bins[2]) else .} %>%
      {if("Snowfall" %in% input$x) filter(.,Snowfall >= input$bins[1] & Snowfall <= input$bins[2]) else .} %>%
      {if("Temperature" %in% input$y) filter (.,Temperature >= input$bins2[1] & Temperature <= input$bins2[2]) else .} %>%
      {if("Rented_Bike_Count" %in% input$y) filter(.,Rented_Bike_Count >= input$bins2[1] & Rented_Bike_Count <= input$bins2[2]) else .} %>%
      {if("Humidity" %in% input$y) filter(.,Humidity >= input$bins2[1] & Humidity <= input$bins2[2]) else .} %>%
      {if("Wind_speed" %in% input$y) filter(.,Wind_speed >= input$bins2[1] & Wind_speed <= input$bins2[2]) else .} %>%
      {if("Visability" %in% input$y) filter(.,Visability >= input$bins2[1] & Visability <= input$bins2[2]) else .} %>%
      {if("Dew_point_temperature" %in% input$y) filter(.,Dew_point_temperature >= input$bins2[1] & Dew_point_temperature <= input$bins2[2]) else .} %>%
      {if("Solar_radiation" %in% input$y) filter(.,Solar_radiation >= input$bins2[1] & Solar_radiation <= input$bins2[2]) else .} %>%
      {if("Rainfall" %in% input$y) filter(.,Rainfall >= input$bins2[1] & Rainfall <= input$bins2[2]) else .} %>%
      {if("Snowfall" %in% input$y) filter(.,Snowfall >= input$bins2[1] & Snowfall <= input$bins2[2]) else .}
    
      # Complete subsetted data + divided by data type
    
      data$a<-subset
    
      subset_num<-subset|>
        select(input$x, input$y)
    
      data$num<-subset_num
    
      subset_cat<-subset|>
        select(names(categorical_my_sample))
    
      data$cat<-subset_cat
    })
  
  # Output for the Data Download tab
  
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
  
  # Output for Data Exploration tab
  
  observeEvent(c(input$submit_Facet_wrap,input$Color), {
    fw<-input$Facet_wrap
    co<-input$Color
    if(fw == co){
      shinyalert(title = "Grouping variables must be different.", type = "error")
      } else {}
  })

  output$corr_scatter<-renderPlot({
    validate(
      need(!is.null(data$a), "Remember to subset first")
    )
    ggplot(data$a, aes_string(x = isolate(input$x), y = isolate(input$y))) +
      geom_point(aes(color = get(input$Color)))+
      facet_wrap(~get(input$Facet_wrap))
  })
  
  output$x_hist<-renderPlot({
    validate(
      need(!is.null(data$a), " ")
    )
    ggplot(data$a, aes_string(x = isolate(input$x))) +
      geom_histogram()
  })
  
  output$y_hist<-renderPlot({
    validate(
      need(!is.null(data$a), " ")
    )
    ggplot(data$a, aes_string(x = isolate(input$y))) +
      geom_histogram()
  })
  
  output$density<-renderPlot({
    validate(
      need(!is.null(data$a), " ")
    )
    ggplot(data$a, aes_string(x = isolate(input$x), y = isolate(input$y))) +
      geom_pointdensity()
  })
  
  output$x_table<-renderTable(apply(data$a|>select(as.numeric(input$num_var)), 2, input$Function))

  output$y_table<-renderTable(apply(data$cat|>select(as.numeric(input$cats)), 2, table))

  output$Corr<-renderPlot({
    ggcorrplot(corr, type = "lower", title = "General Correlation")
  })
}

## Run the application 

shinyApp(ui = ui, server = server)
