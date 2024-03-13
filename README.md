# ibm-unit8-finalproject

**#UI **

# Load libraries
library(shiny)
library(tidyverse)
library(readr)

# Read in data
adult <- read_csv("adult.csv")
# Convert column names to lowercase for convenience 
names(adult) <- tolower(names(adult))

# Application Layout
shinyUI(
  fluidPage(
    br(),
    # TASK 1: Application title
    titlePanel("Trends in Demographic Income"),
    p("Explore the difference between people who earn less than 50K and more than 50K. You can filter the data by country, then explore various demogrphic information."),
    
    # TASK 2: Add first fluidRow to select input for country
    fluidRow(
      column(12, 
             wellPanel(
               selectInput(inputId = "country", "Country:", 
                           c("United-States", "Canada", "Mexico", "Germany", "Philippines")))  # add select input 
             )
    ),
    
    # TASK 3: Add second fluidRow to control how to plot the continuous variables
    fluidRow(
      column(3, 
             wellPanel(
               p("Select a continuous variable and graph type (histogram or boxplot) to view on the right."),
               radioButtons("continuous_variable", "Continuous:", choices = c("age", "hours_per_week")),   # add radio buttons for continuous variables
               radioButtons("graph_type", "Graph:", choices = c("histogram", "boxplot"))    # add radio buttons for chart type
               )
             ),
      column(9, plotOutput("p1"))  # add plot output
    ),
    
    # TASK 4: Add third fluidRow to control how to plot the categorical variables
    fluidRow(
      column(3, 
             wellPanel(
               p("Select a categorical variable to view bar chart on the right. Use the check box to view a stacked bar chart to combine the income levels into one graph. "),
               radioButtons("categorical_variable", "Categorical", choices = c("education","workclass","sex")),    # add radio buttons for categorical variables
               checkboxInput("is_stacked", "Stack Bars", value = FALSE)    # add check box input for stacked bar chart option
               )
             ),
      column(9, plotOutput("p2"))  # add plot output
    )
  )
)


**#SERVER **


# Load libraries
library(shiny)
library(tidyverse)
library(readr)

# Read in data
adult <- read_csv("adult.csv")
# Convert column names to lowercase for convenience 
names(adult) <- tolower(names(adult))

# Define server logic
shinyServer(function(input, output) {
  
  df_country <- reactive({
    adult %>% filter(native_country == input$country)
  })
  
  # TASK 5: Create logic to plot histogram or boxplot
  output$p1 <- renderPlot({
    if (input$graph_type == "histogram") {
      # Histogram
      ggplot(df_country(), aes_string(x =  input$continuous_variable)) +
        geom_histogram(bins = 10) +  # histogram geom
        labs(y = "Number of people", title = paste("Trend of",  input$continuous_variable)) +  # labels
        facet_wrap(~prediction)    # facet by prediction
    }
    else {
      # Boxplot
      ggplot(df_country(), aes_string(y =  input$continuous_variable)) +
        geom_boxplot() +  # boxplot geom
        coord_flip() +  # flip coordinates
        labs(y="Number of people", title = paste("Trend of",  input$continuous_variable) ) +  # labels
        facet_wrap(~prediction)  # facet by prediction
    }
  
  })
  
  # TASK 6: Create logic to plot faceted bar chart or stacked bar chart
  output$p2 <- renderPlot({
    # Bar chart
    p <- ggplot(df_country(), aes_string(x = input$categorical_variable)) +
      geom_bar()+
      labs(x= "Categorical",
           y= "Number of people",
           title= paste("Trend of", input$categorical_variable)) +  # labels
      theme()  # modify theme to change text angle and legend position
    if (input$is_stacked) {
      p + geom_bar(position = "stack")  # add bar geom and use prediction as fill
    }
    else{
      p + 
        geom_bar(fill = input$categorical_variable) + # add bar geom and use input$categorical_variables as fill 
        facet_wrap(.~adult$prediction)   # facet by prediction
    }
  })
  
})

