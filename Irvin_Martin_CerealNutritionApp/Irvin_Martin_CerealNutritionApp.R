library(shiny)
library(tidyverse)
library(shinythemes)

?plotOutput
?mainPanel
?textOutput
?shinytheme
?renderText
?filter
?dataTableOutput
?renderDataTable
?renderTable
?paste

cereals <- read_csv("cereal_dataset.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
   theme = shinytheme("cosmo"),
   
   # Application title
   titlePanel("How Healthy is Your Cereal?"),
   
   navbarPage("Alex Irvin and Caitlin Martin",
              
              tabPanel("Summary",
                       h1("Summary"),
                       h2("What does our app do?"),
                       p("Our app visualizes the relationship between different cereals, their nutritional content, and how different nutrients affect cereal consumer ratings. The app uses dietary characteristics and nutritional information associated with 77 different cereal products."),
                       h1("How do I use this app?"),
                       p("Select one of the tabs at the top of the page...")
                       
              ),
              
              tabPanel("Nutrition Breakdown",
                       
                       # Sidebar with a slider input for number of bins 
                       sidebarLayout(
                          sidebarPanel(
                             selectInput("cereal", 
                                         "Select a cereal to display:",
                                         choices = cereals$name)
                          ),
                          
                          # Show a plot of the generated distribution
                          mainPanel(
                             htmlOutput("nutrition")
                          )
                       )),
             
              
              tabPanel("Statistics",
                       
                       # Sidebar with a slider input for number of bins 
                       sidebarLayout(
                          sidebarPanel(
                             
                             radioButtons("scattercolor", 
                                          "Select scatterplot color:",
                                          choices = c("red","blue","gray50"))
                          ),
                          
                          # Show cereal nutrition information
                          mainPanel(
                             plotOutput("scatter")
                          )
                       ))
              
   )
   
)



# Define server logic required to draw a histogram
server <- function(input, output) {
   
   #renderDataTable(filter(cereals, name == input$cereal))
   output$nutrition <- renderText({paste("<font color=\"#000000\"><b><br>",              #change color to black
                                         "Calories",                                     #title of section
                                         "<font color=\"#FF0000\"><b><br>",              #change color to red
                                         filter(cereals, name == input$cereal)$calories, #filter specific cell
                                         "<br>",                                         #line break
                                         
                                         "<font color=\"#000000\"><b><br>", 
                                         "Fat", 
                                         "<font color=\"#1CCC46\"><b><br>",
                                         filter(cereals, name == input$cereal)$fat,
                                         "<br>",
                                         
                                         "<font color=\"#000000\"><b><br>",
                                         "Sugar", 
                                         "<font color=\"#1CCC46\"><b><br>",
                                         filter(cereals, name == input$cereal)$sugars
                                         
                                 )})
      
   
   output$scatter <- renderPlot({
      
      ggplot(faithful, aes(x = waiting, y = eruptions)) +
         geom_point(color = input$scattercolor) +
         geom_smooth(method = "lm", se = FALSE)
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

