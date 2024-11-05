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
         day=as.numeric(day))

numeric_vars<-c("Rented Bike Count", "Temperature(°C)", "Humidity(%)", "Wind speed (m/s)", "Visibility (10m)",
                "Dew point temperature(°C)", "Solar Radiation (MJ/m2)", "Rainfall(mm)", "Snowfall (cm)")
numeric_my_sample<-my_sample|>
  select(`Rented Bike Count`, `Temperature(°C)`, `Humidity(%)`, `Wind speed (m/s)`, `Visibility (10m)`,
         `Dew point temperature(°C)`, `Solar Radiation (MJ/m2)`, `Rainfall(mm)`, `Snowfall (cm)`)
categorical_my_sample<-my_sample|>
  select(Hour, Seasons, `Functioning Day`, Holiday, month)
# Read in data
ui <- fluidPage(
  
  titlePanel("Seoul Bike Sharing Exploration"),
  sidebarLayout(
    sidebarPanel(
      h2("Choose the numeric subsets of the data:"),
      selectizeInput("corr_x",
                     "x Variable",
                     choices = names(numeric_my_sample)[-1], 
                     selected = names(numeric_my_sample)[2]),
      selectizeInput("corr_y",
                     "y Variable",
                     choices = names(numeric_my_sample)[-2],
                     selected = names(numeric_my_sample)[1]),                  
      h2("Choose the categorical subset of the data:"),
      radioButtons("hhl_seasons",
                   "Seasons",
                   choiceValues = c( 
                                    "Summer",
                                    "Autumn",
                                    "Spring",
                                    "Winter"
                   ),
                   choiceNames = c(
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
      h2("Select a Sample Size"),
      sliderInput("corr_n", "", min = 20, max = 500, value = 20),
      actionButton("corr_sample","Get a Sample!")
    ),
    mainPanel(
      tabsetPanel(id="tabs",
      tabPanel("About",textOutput("about"),
      uiOutput("url"),
      imageOutput("bike")),
      tabPanel("Data Download",
               DT::dataTableOutput("mytable"),downloadButton("downloadData", "Download")),
      tabPanel("Data Exploration",plotOutput("corr_scatter")),
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
)


# Define server logic required to draw a histogram
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
  
  ready<-
  
  output$mytable <- DT::renderDataTable({
    DT::datatable(my_sample|>
                    select(input$corr_y, input$corr_x, names(categorical_my_sample))|>
                    filter(Holiday == input$schl_holi, Seasons == input$hhl_seasons,
                           `Functioning Day` == input$fs_func))
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("bike", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(my_sample|>
                  select(input$corr_y, input$corr_x, names(categorical_my_sample))|>
                  filter(Holiday == input$schl_holi, Seasons == input$hhl_seasons,
                         `Functioning Day` == input$fs_func), file, row.names = FALSE)
    }
  )
  
  
  #################################################3
  ##Correlation tab
  sample_corr <- reactiveValues(corr_data = NULL, corr_truth = NULL)
  
  #update input boxes so they can't choose the same variable
  observeEvent(c(input$corr_x, input$corr_y), {
    corr_x <- input$corr_x
    corr_y <- input$corr_y
    choices <- numeric_vars
    if (corr_x == corr_y){
      choices <- choices[-which(choices == corr_x)]
      updateSelectizeInput(session,
                           "corr_y",
                           choices = choices)
    }
  })
  
  #make sure two variables are selected
  observeEvent(input$corr_sample, {

    corr_vars <- c(input$corr_x, input$corr_y)
    
    subsetted_data <- my_sample |>
      filter(#cat vars first
        Seasons %in% input$hhl_seasons,
        `Functioning Day` %in% input$fs_func,
        Holiday %in% input$schl_holi
      ) 
    
    sample_corr$corr_data <- subsetted_data
    sample_corr$corr_truth <- cor(sample_corr$corr_data |>
                                    select(corr_vars))[1,2]
  })
  
  
  
  #Code for rendering the regression plot. It changes whether a line is requested
  #or not
  output$corr_scatter <- renderPlot({
    validate(
      need(!is.null(sample_corr$corr_data), "")
    )
    ggplot(sample_corr$corr_data, aes_string(x = isolate(input$corr_x), y = isolate(input$corr_y))) +
      geom_point()
  })
  
  
  
  #Code for the correlation guessing game
  observeEvent(input$corr_submit, {
    close <- abs(input$corr_guess - sample_corr$corr_truth) <= .05
    if(close){
      shinyalert(title = "Nicely done!",
                 paste0("The sample correlation is ",
                        round(sample_corr$corr_truth, 4),
                        "."),
                 type = "success"
      )
    } else {
      if(input$corr_guess > sample_corr$corr_truth){
        shinyalert(title = "Try again!",
                   "Try guessing a lower value.")
      } else {
        shinyalert(title = "Try again!",
                   "Try guessing a higher value.")
      }
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

