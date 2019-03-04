library(shiny)
library(tidyverse)
library(shinythemes)
library(ggbiplot)

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

cereals <- read_csv("cereal_dataset.csv") %>% 
  filter(potass > 0 & carbo > 0 & sugars > 0) #remove any negative values
cereals[(4:12)] <- cereals[(4:12)] / cereals[["cups"]] # Normalize all variables to serving size of 1 cup
cereals[] <- lapply(cereals, function(x) if (is.numeric(x)) round(x, 2) else x)
cereals$mfr <- replace(cereals$mfr, cereals$mfr %in% c("A","G","K","N","P","Q","R"), c("American Home Food Products","General Mills", "Kelloggs","Nabisco","Post","Quaker Oats","Ralston Purina"))

#Linear Model
cereals$mfr <- as.factor(cereals$mfr)
cereals$shelf <- as.factor(cereals$shelf)
cereal_lm <- lm(rating ~ calories + protein + fat + sodium + fiber + carbo + sugars + potass +vitamins +shelf + mfr, data = cereals)

#PCA
cereal_gm <- cereals %>% 
  filter(mfr == "General Mills")
cereal_remove <- cereal_gm %>% 
  select(-c(shelf, weight, cups, mfr, type)) %>% #removed columns for PCA analysis b/c not numeric values
        column_to_rownames('name') #change the name to the row name
cereal_pca <- prcomp(cereal_remove, scale = TRUE)

#Graph PCA
ggbiplot(cereal_pca, labels = cereal_gm$name) #PCA for just General Mills #checkbox group, so you can choose how many manufacturers you want to include

#Tab 4 graphs
  ggplot(cereals) +
  geom_point(aes(x = calories, y = rating)) +
               geom_text(aes(label = name, x = calories, y =rating), hjust = 0.2, vjust = 1) 


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
             
              
              tabPanel("Principle Components Analysis (PCA)",
                       
                       # Sidebar with a slider input for number of bins 
                       sidebarLayout(
                          sidebarPanel(
                             #check boxes to show manufacturer in PCA
                             checkboxGroupInput("mfr", 
                                          "Select a manufacturer:",
                                          choices = unique(cereals$mfr)) 
                          ),
                          
                          # Show cereal nutrition information
                          mainPanel(
                             plotOutput("scatter")
                          )
                       )),
              
              
              #Tab 4 will only have radio buttons
              tabPanel("Other Graph Thing",
                       sidebarLayout(
                         sidebarPanel(
                          selectInput("nutrient",
                          "Select a nutrient to graph",
                          choices = c("Calories","Protein","Fat","Sodium","Fiber","Carbohydrate","Sugars","Potassium","Vitamins"))
                         ),
                         # Show cereal nutrition information
                         mainPanel(
                           plotOutput("scatter")
                           )
                         )
                       )
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
      
     #PCA model and graph
     cereal_gm <- cereals %>% 
       filter(mfr == "General Mills")
     cereal_remove <- cereal_gm %>% 
       select(-c(shelf, weight, cups, mfr, type)) %>% #removed columns for PCA analysis b/c not numeric values
       column_to_rownames('name') #change the name to the row name
     cereal_pca <- prcomp(cereal_remove, scale = TRUE)
     
     #Graph PCA
     ggbiplot(cereal_pca, labels = cereal_gm$name)
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

