#Group project: Jacob Hall, Rob Hall, Caitlin Francis, Alyas Khudier, & Kristen McDonald https://www.kaggle.com/competitions/titanic/data
library(dplyr)
library(shiny)
library(readr)
library(ggplot2)
library(plotly)

#load dataset
data = read.csv("train.csv")
data$Pclass <- as.character(data$Pclass) #changes data type so fill color works for class (graph 3)

data$Survived <- factor(data$Survived) #for graphs 5/6


ui <- fluidPage(
  
  titlePanel('Titanic Data'),
  
  #--------------------------------------------------------------------------------------------------------------------------
  #Graph 1                                                  #below are radio button choices
  titlePanel("Embarkment Location Demographics"),
  radioButtons("displaygraph1"," ", c("Fatalities by Embarkment Location" = "0",
                                      "Survivers by Embarkment Location" = "1")), 
  plotOutput("graphic1"),
  mainPanel("C = Cherbourg, Q = Queenstown, S = Southampon"), #This is like a key for the graph
  
  br(), br(), #this creates a small break/space before next graph
  
  #--------------------------------------------------------------------------------------------------------------------------
  #Graph 2
  titlePanel("Age and Ticket Class"), #title above graph
  sidebarLayout(
    sidebarPanel(
      sliderInput("displaygraph2", "Select Class:", #slider info
                  min = 1,
                  max = 3,
                  value =1),
      ("'1' refers to first class and so forth")), #text underneath slider
    mainPanel(
      plotOutput("graphic2"),
    )
  ),
  
  br(), br(), #this creates a small break/space before next graph
  
  #-------------------------------------------------------------------------------------------------------------------------- 
  #Graph 3                                      #below are radio button choices
  titlePanel("Survival by Gender"),
  radioButtons("displaygraph3"," ", c("Fatalities by Gender" = "0",
                                      "Survivers by Gender" = "1")), 
  plotOutput("graphic3"),
  mainPanel("M = male, F = female"), #This is like a key for the graph
  
  br(), br(), #this creates a small break/space before next graph
  
  
  #-------------------------------------------------------------------------------------------------------------------------- 
  #Graph 4 
  titlePanel("Survival by Class"),
  radioButtons("displaygraph4"," ", c("Fatalities by Class" = "0",
                                      "Survival by Class" = "1")),
  plotOutput("graphic4"),
  mainPanel("Class 1 = 1, Class 2 = 2, Class 3 = 3"),
  
  
  br(), br(), #this creates a small break/space before next graph
  
  
  
  
  #-------------------------------------------------------------------------------------------------------------------------- 
  #Graph 5 
  titlePanel("Survival by Age"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("ageRange", "Age Range", min = 0, max = 100, value = c(0, 100), step = 1)),
    mainPanel(
      plotlyOutput("graphic5")
    )
  ),
  
  
  
  br(), br(), #this creates a small break/space before next graph
  #-------------------------------------------------------------------------------------------------------------------------- 
  #Graph 6 
  
  titlePanel("Survivors vs. Fatalities"),
  sidebarLayout(
    sidebarPanel(
      checkboxInput("includeSurvivors", "Include survivors", value = TRUE)),
    mainPanel(
      plotlyOutput("graphic6")
    )
  ),
  
  
  
  br(), br(), #this creates a small break/space before next graph
  #-------------------------------------------------------------------------------------------------------------------------- 
  #Graph 7 
  
  #  titlePanel("Children vs. Passenger Count"),
  #  checkboxInput("dh", "Check to Display a Histogram that shows the Number of Children compared to Passenger Count", FALSE),
  #  verbatimTextOutput("value"),
  #  plotOutput("graphic7"),
  
    
        
  
  br(), br(), #this creates a small break/space before next graph
    #-------------------------------------------------------------------------------------------------------------------------- 
    #Graph 8
  #  titlePanel('Sibling Count Slider by Class'),
  #  sidebarLayout(
  #    sidebarPanel(
  #      sliderInput("graphic8",
  #                  "SELECT Class:",
  #                  min = 1,
  #                  max = 3,
  #                  value = 2)),
  #    mainPanel(
  #      plotOutput("graphic8")
  #    )),
  
  
  
  br(), br(), #this creates a small break/space before next graph
  #-------------------------------------------------------------------------------------------------------------------------- 
  #Graph 9
  
  titlePanel("Survived Titanic Dataset Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Select variable:",
                  choices = c("Sex", "Pclass", "Embarked"),
                  selected = "Sex")
    ),
    mainPanel(
      plotOutput("graphic9")
    )
  ),
  
  
  
  
  
  br(), br(), #this creates a small break/space before next graph
  #-------------------------------------------------------------------------------------------------------------------------- 
  #Graph 10 
  
  titlePanel("Titanic Dataset Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable2", "Select port of embarkation:",
                  choices = c("C", "Q", "S"),
                  selected = "C")
    ),
    mainPanel(
      plotOutput("graphic10")
    )
  )
)




server <- function(input, output) {
  
  #-------------------------------------------------------------------------------------------------------------------------- 
  #Graph 1
  output$graphic1 <- renderPlot({
    #this fills in the 'Embarked' column with unknown for missing data
    data$Embarked <- ifelse(data$Embarked == "C" | data$Embarked == "S" | data$Embarked == "Q", data$Embarked, "Unknown")
    
    ggplot(data[data$Survived==input$displaygraph1,], aes(x=Embarked, fill=Embarked))+
      geom_bar() + ylim(0,450) + labs(title= "Embarkment Location Demographics", x = 'Embarkment Location', y = 'Count of Individuals')
  })
  
  #-------------------------------------------------------------------------------------------------------------------------- 
  #Graph 2
  output$graphic2 <- renderPlot({
    
    dataByClass <- filter(data, data$Pclass==input$displaygraph2) #this selects the data associated with user choice
    
    ggplot(dataByClass, aes(x=Age)) + geom_histogram(col = 'black', fill = 'white') +
      xlim(0,85) + ylim(0,45)
    
  })
  
  #-------------------------------------------------------------------------------------------------------------------------- 
  #Graph 3
  output$graphic3 <- renderPlot({
    
    ggplot(data[data$Survived==input$displaygraph3,], aes(x=Sex, fill=Sex))+
      geom_bar() + ylim(0,500) + labs(title= "Gender Breakdown", x = 'Gender', y = 'Count of Individuals')
  })
  
  
  
  #-------------------------------------------------------------------------------------------------------------------------- 
  #Graph 4
  output$graphic4 <- renderPlot({
    
    ggplot(data[data$Survived==input$displaygraph4,], aes(x=Pclass, fill=Pclass))+
      geom_bar() + ylim(0,380) + labs(title= "Survival by Class", x = 'Class', y = 'Count of Individuals')
    
  })
  
  
  #-------------------------------------------------------------------------------------------------------------------------- 
  #Graph 5
  
  # Create a filtered dataset based on the input values
  filteredData <- reactive({
    data %>% filter(Age >= input$ageRange[1] & Age <= input$ageRange[2])
  })
  
  # Output JPlot1
  output$graphic5 <- renderPlotly({
    filteredData <- filteredData()
    p <- ggplot(filteredData, aes(x = Age, color = Survived)) + geom_density() +
      labs(x = "Age", y = "Density", color = "Survived")
    ggplotly(p, tooltip = c("Age", "Survived"))
  })
  
  
  
  #-------------------------------------------------------------------------------------------------------------------------- 
  #Graph 6
  
  # Filter the data based on the checkbox input
  filteredData2 <- reactive({
    #data <- titanic
    if (!input$includeSurvivors) {
      data <- data[data$Survived == 0, ]
    }
    data
  })
  
  # JPLOT2 Scatter
  output$graphic6 <- renderPlotly({
    data <- filteredData2()
    plot <- plot_ly(data, x = ~Age, y = ~Fare, color = ~Survived,
                    colors = c("red","blue"),
                    type = "scatter", mode = "markers",
                    hovertemplate = paste(
                      "<b>Name:</b> %{text}<br>",
                      "<b>Age:</b> %{x}<br>",
                      "<b>Fare:</b> %{y}<br>",
                      "<b>Survived:</b> %{marker.color}<br>"),
                    text = ~Name)
    plot 
  })
  
  
  
  #-------------------------------------------------------------------------------------------------------------------------- 
  #Graph 7
  output$value <- renderText({ input$dh })
  output$graphic7<-renderPlot({
    if(input$dh){
      ggplot(data,aes(x=Parch, fill=Sex))+ geom_histogram()+
        xlab("Number of Children") + ylab("Passenger ")
    }
  })
  
  
  
  #-------------------------------------------------------------------------------------------------------------------------- 
  #Graph 8
  
  output$graphic8 <- renderPlot({
    dataBySibSp <- filter(data, data$Pclass==input$graphic8)
    ggplot(dataBySibSp, aes(x=SibSp, fill=Sex)) + geom_histogram()+
      xlab("Number of Siblings")+ylab("Passenger Count by Class")
    
  })
  
  
  
  #-------------------------------------------------------------------------------------------------------------------------- 
  #Graph 9
  
  # Create the plot based on user input

  output$graphic9 <- renderPlot({
    ggplot(data, aes_string(x = input$variable, fill = "Survived")) +
      geom_bar(position = "dodge") +
      labs(x = input$variable, y = "Count", fill = "Survived") +
      theme_minimal()
  })
  
  
  
  #-------------------------------------------------------------------------------------------------------------------------- 
  #Graph 10
  
  # Create the plot based on user input
  output$graphic10 <- renderPlot({
    ggplot(data[data$Embarked == input$variable2,], aes(x = Age, y = Fare, color = Sex)) +
      geom_point() +
      labs(x = "Age", y = "Fare", color = "Sex") +
      theme_minimal()
  })
  
  
}

shinyApp(ui, server)
