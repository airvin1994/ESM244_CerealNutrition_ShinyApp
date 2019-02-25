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
cereals[(4:12)] <- cereals[(4:12)] / cereals[["cups"]] # Normalize all variables to serving size of 1 cup
cereals[] <- lapply(cereals, function(x) if (is.numeric(x)) round(x, 2) else x)
cereals$mfr <- replace(cereals$mfr, cereals$mfr %in% c("A","G","K","N","P","Q","R"), c("American Home Food Products","General Mills", "Kelloggs","Nabisco","Post","Quaker Oats","Ralston Purina"))

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
                                         choices = cereals$name),
                             htmlOutput("baseline")
                          ),
                          
                          # Show a plot of the generated distribution
                          mainPanel(
                           htmlOutput("nutrition"),
                           htmlOutput("nutrition_2")
                          )
                       )),
             
              
              tabPanel("Statistics",
                       
                       # Sidebar with a slider input for number of bins 
                       sidebarLayout(
                          sidebarPanel(
                             
                             selectInput("nutrient",
                                         "Select a nutrient to graph",
                                         choices = c("Calories","Protein","Fat","Sodium","Fiber","Carbohydrate","Sugars","Potassium","Vitamins")),
                             radioButtons("scattercolor", 
                                          "Select a manufacturer:",
                                          choices = unique(cereals$mfr))
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

   output$baseline <- renderText({paste("<font size = 4>",
                                        "<b>Baseline healthy cereal:<b>",
                                        "<font size = 2>",
                                        "<br><br>",
                                        "Calories: 100",
                                        "<br><br>",
                                        "Fat (g): 0",
                                        "<br><br>",
                                        "Sugar (g): 0",
                                        "<br><br>",
                                        "Sodium (mg): 280",
                                        "<br><br>",
                                        "Fiber (g): 28",
                                        "<br><br>",
                                        "Vitamins & Minerals: 50%",
                                        "<br><br>",
                                        "Potassium (mg): 660",
                                        "<br><br>",
                                        "Protein (g): 4",
                                        "<br><br>",
                                        "Complex Carbohydrates (g): 16",
                                        "<br><br>",
                                        "Nutrition Rating: 93.70",
                                        "<br><br>",
                                        "Shelf Display Location: 3",
                                        "<br><br>",
                                        "Manufacturer: Nabisco"
                                        
                                        
                                        )})
   #renderDataTable(filter(cereals, name == input$cereal))
   output$nutrition <- renderText({paste( "<font size = 5>",                     #change font size
                                          "<font color=\"#000000\"><b><br>",       #change color to black
                                         "Calories (per 1 cup serving)",         #title of section
                                         "<font color=\"#FF0000\"><b><br>",             #change color to red
                                         filter(cereals, name == input$cereal)$calories, #filter specific cell
                                         "<br>",                                         #line break
                                         
                                         "<font color=\"#000000\"><b><br>", 
                                         "Fat (g)", 
                                         "<font color=\"#1CCC46\"><b><br>",
                                         filter(cereals, name == input$cereal)$fat,
                                         "<br>",
                                         
                                         "<font color=\"#000000\"><b><br>",
                                         "Sugar (g)", 
                                         "<font color=\"#1CCC46\"><b><br>",
                                         filter(cereals, name == input$cereal)$sugars,
                                         "<br>",
                                         
                                         "<font color=\"#000000\"><b><br>",
                                         "Sodium (mg)", 
                                         "<font color=\"#1CCC46\"><b><br>",
                                         filter(cereals, name == input$cereal)$sodium, 
                                         "<br>",
                                         
                                         "<font color=\"#000000\"><b><br>",
                                         "Fiber (g)", 
                                         "<font color=\"#1CCC46\"><b><br>",
                                         filter(cereals, name == input$cereal)$fiber, 
                                         "<br>", 
                                         
                                         "<font color=\"#000000\"><b><br>",
                                         "Vitamins & Minerals (typical percentage of FDA recommended consumption)", 
                                         "<font color=\"#1CCC46\"><b><br>",
                                         filter(cereals, name == input$cereal)$vitamins, 
                                         "<br>", 
                                         
                                         "<font color=\"#000000\"><b><br>",
                                         "Potassium (mg)", 
                                         "<font color=\"#1CCC46\"><b><br>",
                                         filter(cereals, name == input$cereal)$potass, 
                                         "<br>", 
                                         
                                         "<font color=\"#000000\"><b><br>",
                                         "Protein (g)", 
                                         "<font color=\"#1CCC46\"><b><br>",
                                         filter(cereals, name == input$cereal)$protein, 
                                         "<br>", 
                                         
                                         "<font color=\"#000000\"><b><br>",
                                         "Complex Carbohydrates (g)", 
                                         "<font color=\"#1CCC46\"><b><br>",
                                         filter(cereals, name == input$cereal)$carbo, 
                                         "<br>", 
                                         
                                         "<font color=\"#000000\"><b><br>",
                                         "Nutrition Rating", 
                                         "<font color=\"#1CCC46\"><b><br>",
                                         filter(cereals, name == input$cereal)$rating, 
                                         "<br>", 
                                         
                                         "<font color=\"#000000\"><b><br>",
                                         "Shelf Display Location (1, 2, or 3, counting from the floor)", 
                                         "<font color=\"#1CCC46\"><b><br>",
                                         filter(cereals, name == input$cereal)$shelf, 
                                         "<br>", 
                                         
                                         "<font color=\"#000000\"><b><br>",
                                         "Manufacturer", 
                                         "<font color=\"#1CCC46\"><b><br>",
                                         filter(cereals, name == input$cereal)$mfr, 
                                         "<br>"
                                 )})
   output$nutrition_2 <- renderText({paste("test")})
      
   
   output$scatter <- renderPlot({
      
      ggplot(faithful, aes(x = waiting, y = eruptions)) +
         geom_point(color = input$scattercolor) +
         geom_smooth(method = "lm", se = FALSE)
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

