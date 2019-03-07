library(shiny)
library(tidyverse)
library(shinythemes)
library(ggbiplot)

cereals <- read_csv("cereal_dataset.csv") %>% 
  filter(potass >= 0 & carbo >= 0 & sugars >= 0) #remove any negative values
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
                           htmlOutput("nutrition")
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
                             plotOutput("pca")
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



# Define server logic
server <- function(input, output) {
   
   #Vector with just the baseline cereal information
   baseline_cereal <- filter(cereals, name == "All-Bran with Extra Fiber")
   
   #Output for baseline cereal nutrition info
   output$baseline <- renderText({
      paste("<font size = 4>",
            "<b>Baseline healthy cereal:<b>",
            "<font size = 2>",
            "<br><br>",
            paste("Calories: ", baseline_cereal$calories),
            "<br><br>",
            paste("Fat (g): ", baseline_cereal$fat),
            "<br><br>",
            paste("Sugar (g): ", baseline_cereal$sugars),
            "<br><br>",
            paste("Sodium (mg): ", baseline_cereal$sodium),
            "<br><br>",
            paste("Fiber (g): ", baseline_cereal$fiber),
            "<br><br>",
            paste("Vitamins and Minerals: ", baseline_cereal$vitamins),
            "<br><br>",
            paste("Potassium (mg): ", baseline_cereal$potass),
            "<br><br>",
            paste("Protein (g): ", baseline_cereal$protein),
            "<br><br>",
            paste("Carbohydrates (g): ", baseline_cereal$carbo),
            "<br><br>",
            paste("Nutrition Rating: ", baseline_cereal$rating),
            "<br><br>",
            paste("Shelf Display Location: ", baseline_cereal$shelf),
            "<br><br>",
            paste("Manufacturer: ", baseline_cereal$mfr)
            )})
   
   
   #Output for selected cereal nutrition info
   output$nutrition <- renderText({
      
      #Set color based on selection
      #Green = #008000	
      #Red = #FF0000
      #Black = #000000
      
      selected_cereal <- filter(cereals, name == input$cereal)
      color_vector <- vector()
      
      for (i in 4:length(selected_cereal))
      {
         color_vector <- c(color_vector, ifelse(as.numeric(selected_cereal[1,i]) > as.numeric(baseline_cereal[1,i]),"FF0000\\","008000\\"))
      }
      
      #Print out nutrition info
      paste( "<font size = 5>", #change font size
            "<font color=\"#000000\"><b><br>",                     #change color to black
            "Calories (per 1 cup serving)",                        #title of section
            paste0("<font color=\\",color_vector[1],"><b><br>"),  #change color to red
            filter(cereals, name == input$cereal)$calories,        #filter specific cell
            "<br>",                                                #line break
                                         
            "<font color=\"#000000\"><b><br>", 
            "Fat (g)", 
            paste0("<font color=\\",color_vector[3],"><b><br>"),
            filter(cereals, name == input$cereal)$fat,
            "<br>",
                                         
            "<font color=\"#000000\"><b><br>",
            "Sugar (g)", 
            paste0("<font color=\\",color_vector[7],"><b><br>"),
            filter(cereals, name == input$cereal)$sugars,
            "<br>",
                                         
            "<font color=\"#000000\"><b><br>",
            "Sodium (mg)", 
            paste0("<font color=\\",color_vector[4],"><b><br>"),
            filter(cereals, name == input$cereal)$sodium, 
            "<br>",
                                         
            "<font color=\"#000000\"><b><br>",
            "Fiber (g)", 
            paste0("<font color=\\",color_vector[5],"><b><br>"),
            filter(cereals, name == input$cereal)$fiber, 
            "<br>", 
                                         
            "<font color=\"#000000\"><b><br>",
            "Vitamins & Minerals (typical percentage of FDA recommended consumption)", 
            paste0("<font color=\\",color_vector[9],"><b><br>"),
            filter(cereals, name == input$cereal)$vitamins, 
            "<br>", 
                                         
            "<font color=\"#000000\"><b><br>",
            "Potassium (mg)", 
            paste0("<font color=\\",color_vector[8],"><b><br>"),
            filter(cereals, name == input$cereal)$potass, 
            "<br>", 
                                         
            "<font color=\"#000000\"><b><br>",
            "Protein (g)", 
            paste0("<font color=\\",color_vector[2],"><b><br>"),
            filter(cereals, name == input$cereal)$protein, 
            "<br>", 
                                         
            "<font color=\"#000000\"><b><br>",
            "Complex Carbohydrates (g)", 
            paste0("<font color=\\",color_vector[6],"><b><br>"),
            filter(cereals, name == input$cereal)$carbo, 
            "<br>", 
                                         
            "<font color=\"#000000\"><b><br>",
            "Nutrition Rating", 
            paste0("<font color=\\",color_vector[13],"><b><br>"),
            filter(cereals, name == input$cereal)$rating, 
            "<br>", 
                                         
            "<font color=\"#000000\"><b><br>",
            "Shelf Display Location (1, 2, or 3, counting from the floor)", 
            paste0("<font color=\\",color_vector[10],"><b><br>"),
            filter(cereals, name == input$cereal)$shelf, 
            "<br>", 
                                         
            "<font color=\"#000000\"><b><br>",
            "Manufacturer", 
            "<font color=\"#008000\"><b><br>",
            filter(cereals, name == input$cereal)$mfr, 
            "<br>"
            )})
   
   
   output$pca <- renderPlot({
      
      #PCA model and graph
      cereal_gm <- cereals %>%
         subset(mfr %in% input$mfr) #filter by manufacturer
      cereal_remove <- cereal_gm %>% 
         select(-c(shelf, weight, cups, mfr, type)) %>% #removed columns for PCA analysis b/c not numeric values
         column_to_rownames('name') #change the name to the row name
      cereal_pca <- prcomp(cereal_remove, scale = TRUE)
   
     
      #Graph PCA
      ggbiplot(cereal_pca, labels = cereal_gm$name) + theme_classic()
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

