library(tidyverse)
library(ggpointdensity)
library(ggupset)
library(ggcorrplot)
library(shiny)
library(shinyalert)



my_sample_initial<-read_csv("Seoul_Bike_Data.csv")
my_sample_initial<-my_sample_initial|>
  mutate(month=str_split_i(Date,'/',2),
         month=as.numeric(month),day=str_split_i(Date,'/',1),
         day=as.numeric(day))
my_sample<-my_sample_initial|>
  filter(`Functioning Day`== 'Yes')

cat_vars<-c("Hour", "Seasons","Holiday", "month")
numeric_vars<-c("Rented Bike Count", "Temperature(°C)")

# Read in data
ui <- fluidPage(
  
  titlePanel("Seoul Bike Sharing Exploration"),
  sidebarLayout(
    sidebarPanel(
      h2("Choose the categorical subset of the data:"),
      radioButtons("seasons_corr",
                   "Season",
                   choiceValues = c("all", 
                                    "spring",
                                    "summer",
                                    "autumn",
                                    "winter"
                   ),
                   choiceNames = c("all", 
                                   "spring",
                                   "summer",
                                   "autumn",
                                   "winter"
                   )
      ),
      radioButtons("holiday_corr",
                   "Holiday",
                   choiceValues = c( 
                     "yes",
                     "no"
                   ),
                   choiceNames = c(
                     "Yes",
                     "No"
                   )
      ),
      h2("Choose the numeric subsets of the data:"),
      selectizeInput("corr_x",
                     "x Variable",
                     choices = numeric_vars[-1], 
                     selected = numeric_vars[2]),
      sliderInput("n1", "n1", min = min(my_sample$`Temperature(°C)`), max=max(my_sample$`Temperature(°C)`), value=mean(my_sample$`Temperature(°C)`)),
      selectizeInput("corr_y",
                     "y Variable",
                     choices = numeric_vars[-2],
                     selected = numeric_vars[1]),                  
      actionButton("corr_sample","Subset!")
    ),
    mainPanel(
      textOutput("about"),
      uiOutput("url"),
      imageOutput("bike"),
      conditionalPanel("input.corr_sample",
                       h2("Guess the correlation!"),
                       column(6, 
                              numericInput("corr_guess",
                                           "",
                                           value = 0,
                                           min = -1, 
                                           max = 1
                              )
                       ),
                       column(6, 
                              actionButton("corr_submit", "Check Your Guess!"))
      )
    )
  )
)



# Define server logic ----
server <- function(input, output, session) {
  output$about <- renderText({
    "This app was created to explore the Seoul Bike Sharing Demand Prediction data set."
  })
  output$url<-renderUI({
    tagList("",a("Seoul Bike Sharing Data", href="https://www.kaggle.com/datasets/saurabhshahane/seoul-bike-sharing-demand-prediction/data") )
  })
  output$bike<-renderImage({ 
    filename <- normalizePath(file.path('./images','Bike.png'))
    
    list(src = filename,
         contentType = 'image/png',
         alt = "")
  }, deleteFile = FALSE)
  

  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
